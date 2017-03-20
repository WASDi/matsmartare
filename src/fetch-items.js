import request from 'request';
import cheerio from 'cheerio';
import sqlite3 from 'sqlite3';

const baseURL = "http://www.matsmart.se";

let db = new sqlite3.Database("matsmartare.db");
db.all("SELECT id, url, title FROM categories", function(err, rows) {
  rows.forEach(function(row) {
    if (row.id != 6) {
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
      products.each(function(id, element) {
        const itemURL = element.attribs.href;
        const itemImageURL = "http:" + $(element).find("img.zoom").first()[0].attribs.src.replace(/\?itok=.*/g, '');
        const name = $(element).find("span.prd-name").first().text().replace(/^\s+|\s+$/g, '');
        const price = $(element).find("div.prd-price-num").first().text().match(/\d+/); //FIXME doesn't understand that it's a sum price
        const discount = $(element).find("span.prd-discount-oldprice > span").first().text().replace(/^[^\(]+..|..[^\)]+$/g, '');
        // TODO insert or update db
      });
    });
  })
});
db.close();
