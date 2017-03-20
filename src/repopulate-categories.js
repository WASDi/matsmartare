import request from 'request';
import cheerio from 'cheerio';
import sqlite3 from 'sqlite3';

const extURL = "http://www.matsmart.se/";
request(extURL, function(error, response, html) {
  if (error) {
    console.log(error);
    return;
  }
  const $ = cheerio.load(html);
  const menuItems = $("#category-menu > li > a");

  if (!menuItems || menuItems.length == 0) {
    console.log("menuItems empty?");
    return;
  }

  let db = new sqlite3.Database("matsmartare.db");
  db.run("BEGIN TRANSACTION");
  db.run("DELETE FROM categories");
  menuItems.each(function(id, element) {
    let categoryUrl = element.attribs.href;
    let categoryName = element.children[0].data;
    // console.log(categoryUrl + " ... " + categoryName);
    db.run("INSERT INTO categories (id, url, name) VALUES (?,?,?)", id, categoryUrl, categoryName);
  });
  db.run("COMMIT");
  db.close();
});
