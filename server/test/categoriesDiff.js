import request from 'request';
import cheerio from 'cheerio';
import sqlite3 from 'sqlite3';

import { newCategory } from "../src/lib/models.js";

const db = new sqlite3.Database("matsmartare.db");
db.all("SELECT id, url, title FROM categories", function(err, rows) {
  const dbCategories = rows.map(x => newCategory(x.id, x.url, x.title));
  console.log(dbCategories);

  // const extURL = "http://www.matsmart.se/";
  //
  // request(extURL, function(error, response, html) {
  //
  // });


});
