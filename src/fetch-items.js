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
// INSERT INTO items (category_id, url, img_url, name, price, discount, first_seen, last_seen) VALUES (1, 'my_url', 'my_img_url', 'my_name', 49.50, 10, datetime(CURRENT_TIMESTAMP,'localtime'), datetime(CURRENT_TIMESTAMP,'localtime'))
let db = new sqlite3.Database("matsmartare.db");
/*
db.all("SELECT * FROM items", function(err, rows) {
  rows.forEach(function(row) {
    let item = newItem(row.id, row.category_id, row.url, row.img_url, row.name, row.price, row.discount, row.first_seen, row.last_seen);
    console.log(item);
  });
});
*/

//db.run("BEGIN TRANSACTION");
db.serialize(function() {
  db.each("SELECT id, url, title FROM categories", function(err, row) {
    const categoryId = row.id;
    if (categoryId != 6) {
      return true; // Be nice when developing, only process one url bellow
    }
    const url = baseURL + row.url;
    request(url, function(error, response, html) {
      if (error) {
        console.log(error);
        return;
      }
      const $ = cheerio.load(html);
      const products = $("div.prd > a");
      //FIXME items can have many categories
      db.run("BEGIN TRANSACTION");
      let stmt = db.prepare("INSERT INTO items (category_id, url, img_url, name, price, discount, first_seen, last_seen) VALUES (?, ?, ?, ?, ?, ?, ?, ?)");
      //products.each(function(id, element) {
      for (let i = 0; i < products.length; i++) {
        const element = products[i];
        const itemURL = element.attribs.href;
        const itemImageURL = "http:" + $(element).find("img.zoom").first()[0].attribs.src.replace(/\?itok=.*/g, '');
        const name = $(element).find("span.prd-name").first().text().replace(/^\s+|\s+$/g, '');
        const price = $(element).find("div.prd-price-num").first().text().match(/\d+/)[0]; //FIXME doesn't understand that it's a sum price
        const discount = $(element).find("span.prd-discount-oldprice > span").first().text().replace(/^[^\(]+..|..[^\)]+$/g, '');
        //console.log("..." + price);
        stmt.run(categoryId, itemURL, itemImageURL, name, price, discount, TIMESTAMP_NOW, TIMESTAMP_NOW);
      }
      //});
      db.run("COMMIT");
    });

  });
});

// db.close();
