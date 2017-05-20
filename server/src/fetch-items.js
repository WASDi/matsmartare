import request from 'request';
import cheerio from 'cheerio';
import sqlite3 from 'sqlite3';
import HashMap from 'hashmap';
import parseItem from './lib/parseItem.js';
import {
  fetchItemsFromDb,
  resolveCategories
} from './lib/fetch-items-from-db.js';

const baseURL = "http://www.matsmart.se";
const TIMESTAMP_NOW = Math.round(new Date().getTime() / 1000);

function promiseFunctionsForFetchingItems(categories) {
  let tasks = [];
  categories.forEach(function(category) {
    var promiseFunc = () => new Promise((resolve, reject) => {
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
    tasks.push(promiseFunc);
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

async function runSerial(tasks) {
  const results = [];
  for(let i = 0; i < tasks.length; i++) {
    const result = await tasks[i]();
    results.push(result);
  }
  return results;
}

function fetchItemsFromMatsmart(db) {
  return new Promise((resolve, reject) => {
    db.all("SELECT id, url, name FROM categories", function(err, rows) {
      if (err) {
        reject(err);
      }
      let categories = resolveCategories(rows);
      let tasks = promiseFunctionsForFetchingItems(categories);

      runSerial(tasks).then(values => {
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
      let insertNewStmt = db.prepare("INSERT INTO items (categories, url, img_url, name, price, discount, best_before, first_seen, last_seen) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)");
      let updateStmt = db.prepare("UPDATE items SET categories = ?, img_url = ?, price = ?, discount = ?, best_before = ?, last_seen = ? WHERE id = ?");

      matsmartItems.forEach(function(item) {
        let dbItem = dbItems.get(item.url);
        if (dbItem == undefined) {
          insertNewStmt.run(item.categories.join(","), item.url, item.img_url, item.name, item.price, item.discount, item.best_before, TIMESTAMP_NOW, TIMESTAMP_NOW);
          result.newItems++;
        } else {
          updateStmt.run(item.categories.join(","), item.img_url, item.price, item.discount, item.best_before, TIMESTAMP_NOW, dbItem.id);
          result.updatedItems++;
        }
      });

      // archive items not seen for 15 hours
      db.run("UPDATE items SET name = 'ARCHIVE_' || name WHERE last_seen < (SELECT max(last_seen)-(15*3600) FROM items) AND name NOT LIKE 'ARCHIVE_%'");

      insertNewStmt.finalize();
      updateStmt.finalize();
      db.run("COMMIT");

      resolve(result);
    });
  });
}

function insertUpdateLog(db, numWebItems, numNewItems) {
  return new Promise((resolve, reject) => {
    db.serialize(function() {
      db.run("BEGIN TRANSACTION");
      const insertStmt = db.prepare("INSERT INTO update_logs (when_timestamp, num_web_items, num_new_items) VALUES (?, ?, ?)")
      insertStmt.run(TIMESTAMP_NOW, numWebItems, numNewItems);
      insertStmt.finalize();
      db.run("COMMIT");
      resolve(null);
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
  await insertUpdateLog(db, matsmartItems.length, result.newItems);
  console.log("Result: " + result.newItems + " new items and " + result.updatedItems + " updates.");

  db.close();
}

execute().then(
  success => {},
  fail => {console.log("FAIL: " + fail)}
);

// process.on('unhandledRejection', (reason) => {
//     console.log(reason);
// });
