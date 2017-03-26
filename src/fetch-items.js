import request from 'request';
import cheerio from 'cheerio';
import sqlite3 from 'sqlite3';
import HashMap from 'hashmap';
import fetchItemsFromDb from './fetch-items-from-db.js';
import {
  newItem,
  newCategory
} from './models.js';

const baseURL = "http://www.matsmart.se";
const TIMESTAMP_NOW = Math.round(new Date().getTime() / 1000);

function parseItem($, element, categoryId) {
  const itemURL = element.attribs.href;
  const itemImageURL = "http:" + $(element).find("img.zoom").first()[0].attribs.src.replace(/\?itok=.*/g, '');
  const name = $(element).find("span.prd-name").first().text().replace(/^\s+|\s+$/g, '');
  const discount = $(element).find("span.prd-discount-oldprice > span").first().text().replace(/^[^\(]+..|..[^\)]+$/g, '');

  let price = $(element).find("div.prd-price-num").first().text().match(/\d+/)[0];
  const itemsForPriceElem = $(element).find("span.prd-mp-text").first();
  if (itemsForPriceElem.length != 0) {
    const itemsForPrice = itemsForPriceElem.text().match(/\d+/)[0];
    price = price / itemsForPrice;
  }

  return newItem(-1, [categoryId], itemURL, itemImageURL, name, price, discount, 0, 0);
}

function resolveCategories(rows) {
  let categories = [];
  rows.forEach(function(row) {
    const categoryId = row.id;
    if (!(row.id == 4 || row.id == 555555)) {
      //return true; // Be nice when developing, do few http requests
    }
    categories.push(newCategory(row.id, row.url, row.title));
  });
  return categories;
}

function promisesForFetchingItems(categories) {
  let tasks = [];
  categories.forEach(function(category) {
    var promise = new Promise((resolve, reject) => {
      const url = baseURL + category.url;
      request(url, function(error, response, html) {
        if (error) {
          reject(error);
          return;
        }
        const $ = cheerio.load(html);
        const products = $("div.prd > a");
        let items = [];
        products.each(function(id, element) {
          let item = parseItem($, element, category.id);
          items.push(item);
        });
        resolve(items);
      });
    });
    tasks.push(promise);
  });
  return tasks;
}

function flatMapCombineCategories(categoryItems) {
  let url2item = new HashMap();

  categoryItems.forEach(function(itemsForCategory) {
    itemsForCategory.forEach(function(itemSingleCategory) {
      let itemFromMap = url2item.get(itemSingleCategory.url);
      if (itemFromMap == undefined) {
        url2item.set(itemSingleCategory.url, itemSingleCategory);
      } else {
        itemFromMap.categories.push(itemSingleCategory.categories[0]);
      }
    });
  });

  return url2item.values();
}

function fetchItemsFromMatsmart(db) {
  return new Promise((resolve, reject) => {
    db.all("SELECT id, url, title FROM categories", function(err, rows) {
      if (err) {
        reject(err);
      }
      let categories = resolveCategories(rows);
      let tasks = promisesForFetchingItems(categories);

      Promise.all(tasks).then(values => {
        try {
          resolve(flatMapCombineCategories(values));
        } catch (err) {
          reject(err);
        }
      }, fail => {
        reject(fail);
      });
    });
  });
}

function mergeProcessItems(db, dbItems, matsmartItems) {
  return new Promise((resolve, reject) => {
    db.serialize(function() {
      let result = {
        newItems: 0,
        updatedItems: 0
      };
      db.run("BEGIN TRANSACTION");
      let insertNewStmt = db.prepare("INSERT INTO items (categories, url, img_url, name, price, discount, first_seen, last_seen) VALUES (?, ?, ?, ?, ?, ?, ?, ?)");
      let updateStmt = db.prepare("UPDATE items SET categories = ?, price = ?, discount = ?, last_seen = ? WHERE id = ?");

      matsmartItems.forEach(function(item) {
        let dbItem = dbItems.get(item.url);
        if (dbItem == undefined) {
          insertNewStmt.run(item.categories.join(","), item.url, item.img_url, item.name, item.price, item.discount, TIMESTAMP_NOW, TIMESTAMP_NOW);
          result.newItems++;
        } else {
          updateStmt.run(item.categories.join(","), item.price, item.discount, TIMESTAMP_NOW, dbItem.id);
          result.updatedItems++;
        }
      });

      insertNewStmt.finalize();
      updateStmt.finalize();
      db.run("COMMIT");

      resolve(result);
    });
  });
}

async function execute() {
  let db = new sqlite3.Database("matsmartare.db");

  const dbItems = await fetchItemsFromDb(db, true);
  console.log("Items in db: " + dbItems.count());

  const matsmartItems = await fetchItemsFromMatsmart(db);
  console.log("Items from web: " + matsmartItems.length);

  const result = await mergeProcessItems(db, dbItems, matsmartItems);
  console.log("Result: " + result.newItems + " new items and " + result.updatedItems + " updates.");

  db.close();
}

execute();
