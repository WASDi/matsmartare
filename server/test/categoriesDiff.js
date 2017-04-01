import request from 'request';
import cheerio from 'cheerio';
import sqlite3 from 'sqlite3';

import {
  newCategory
} from "../src/lib/models.js";
import fetchCategories from '../src/lib/fetch-categories.js';

function findDiff(dbCategories, webCategories) {
  if (dbCategories.length !== webCategories.length) {
    console.log(" ##### DIFFERENT LENGTH #####");
    return;
  }
  let diffFound = false;
  const len = dbCategories.length;
  for (let i = 0; i < len; i++) {
    const dbCat = dbCategories[i];
    const webCat = webCategories[i];
    if (dbCat.url !== webCat.url) {
      console.log(" ##### URL DIFF: " + dbCat.url + ", " + webCat.url);
      diffFound = true;
    } else if (dbCat.title !== webCat.title) {
      console.log(" ##### TITLE DIFF: " + dbCat.title + ", " + webCat.title);
      diffFound = true;
    }
  }
  if(!diffFound) {
    console.log(" ----- no diffs -----");
  }
}

const db = new sqlite3.Database("matsmartare.db");
db.all("SELECT id, url, title FROM categories", function(err, rows) {
  const dbCategories = rows.map(x => newCategory(x.id, x.url, x.title));

  fetchCategories(function(webCategories) {
    findDiff(dbCategories, webCategories);
    db.close();
  });
});
