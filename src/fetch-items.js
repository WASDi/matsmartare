import request from 'request';
import cheerio from 'cheerio';
import sqlite3 from 'sqlite3';

const baseURL = "http://www.matsmart.se";
const TIMESTAMP_NOW = Math.round(new Date().getTime() / 1000);

function newItem(id, category_id, url, img_url, name, price, discount, first_seen, last_seen) {
  return {
    id,
    category_id,
    url,
    img_url,
    name,
    price,
    discount,
    first_seen,
    last_seen
  };
}

function newCategory(id, url, title) {
  return {
    id,
    url,
    title
  };
}

function parseItem($, element, categoryId) {
  const itemURL = element.attribs.href;
  const itemImageURL = "http:" + $(element).find("img.zoom").first()[0].attribs.src.replace(/\?itok=.*/g, '');
  const name = $(element).find("span.prd-name").first().text().replace(/^\s+|\s+$/g, '');
  const price = $(element).find("div.prd-price-num").first().text().match(/\d+/)[0]; //FIXME doesn't understand that it's a sum price
  const discount = $(element).find("span.prd-discount-oldprice > span").first().text().replace(/^[^\(]+..|..[^\)]+$/g, '');

  return newItem(-1, categoryId, itemURL, itemImageURL, name, price, discount, TIMESTAMP_NOW, TIMESTAMP_NOW);
}

function resolveCategories(rows) {
  let categories = [];
  rows.forEach(function(row) {
    const categoryId = row.id;
    if (row.id != 19) {
      return true; // Be nice when developing, only process one url bellow
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
        //FIXME items can have many categories
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

function fetchItemsFromDb(db) {
  let items = [];
  db.each("SELECT * FROM items", function(err, row) {
      let item = newItem(row.id, row.category_id, row.url, row.img_url, row.name, row.price, row.discount, row.first_seen, row.last_seen);
      items.push(item);
  });
  //TODO return above as promise
}

function fetchItemsFromMatsmart(db, callback) {
  db.all("SELECT id, url, title FROM categories", function(err, rows) {
    let categories = resolveCategories(rows);

    let tasks = promisesForFetchingItems(categories);

    Promise.all(tasks).then(values => {
      callback(categories, values);
      db.close();
    }, fail => {
      console.log(fail);
      db.close();
    });
  });
}

// INSERT INTO items (category_id, url, img_url, name, price, discount, first_seen, last_seen) VALUES (1, 'my_url', 'my_img_url', 'my_name', 49.50, 10, datetime(CURRENT_TIMESTAMP,'localtime'), datetime(CURRENT_TIMESTAMP,'localtime'))
let db = new sqlite3.Database("matsmartare.db");

// db.run("BEGIN TRANSACTION");
// let stmt = db.prepare("INSERT INTO items (category_id, url, img_url, name, price, discount, first_seen, last_seen) VALUES (?, ?, ?, ?, ?, ?, ?, ?)");
// stmt.run(item.category_id, itemURL, itemImageURL, name, price, discount, TIMESTAMP_NOW, TIMESTAMP_NOW);
// db.run("COMMIT");

db.serialize(function() {
  fetchItemsFromDb(db); // TODO return hashmap of id -> item, then parse diff bellow and update db?
  fetchItemsFromMatsmart(db, function(categories, values){
    for (let i = 0; i < values.length; i++) {
      console.log("Category '" + categories[i].title + "' has " + values[i].length + " items.");
    }
  });
});
