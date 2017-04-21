import request from 'request';
import cheerio from 'cheerio';
import sqlite3 from 'sqlite3';

import {
  newCategory
} from "../src/lib/models.js";
import fetchCategories from '../src/lib/fetch-categories.js';

function findDiff(dbCategories, webCategories) {
  let diffFound = false;
  if (dbCategories.length !== webCategories.length) {
    console.log(" ##### DIFFERENT LENGTH ##### DB = " + dbCategories.length + ", WEB = " + webCategories.length);
    diffFound = true;
  }

  // check everything in DB, if it also exists in WEB
  for (let db_idx = 0; db_idx < dbCategories.length; db_idx++) {
    const dbCat = dbCategories[db_idx];
    let webAlsoHasUrl = false;
    let webAlsoHasName = false;
    for (let web_idx = 0; web_idx < webCategories.length; web_idx++) {
      const webCat = webCategories[web_idx];
      if (dbCat.url === webCat.url) {
        webAlsoHasUrl = true;
      }
      if (dbCat.name === webCat.name) {
        webAlsoHasName = true;
      }
    }

    if (!webAlsoHasUrl || !webAlsoHasName) {
      console.log(" ##### DIFF: DB has " + dbCat.name + " with url " + dbCat.url + " but WEB does not");
      diffFound = true;
    }
  }

  // check everything in WEB, if it also exists in DB
  for (let web_idx = 0; web_idx < webCategories.length; web_idx++) {
    const webCat = webCategories[web_idx];
    let dbAlsoHasUrl = false;
    let dbAlsoHasName = false;
    for (let db_idx = 0; db_idx < dbCategories.length; db_idx++) {
      const dbCat = dbCategories[db_idx];
      if (dbCat.url === webCat.url) {
        dbAlsoHasUrl = true;
      }
      if (dbCat.name === webCat.name) {
        dbAlsoHasName = true;
      }
    }

    if (!dbAlsoHasUrl || !dbAlsoHasName) {
      console.log(" ##### DIFF: WEB has " + webCat.name + " with url " + webCat.url + " but DB does not");
      diffFound = true;
    }
  }

  if (!diffFound) {
    console.log(" ----- no diffs -----");
  }
}

const db = new sqlite3.Database("matsmartare.db");
db.all("SELECT id, url, name FROM categories", function(err, rows) {
  if (err) {
    console.log(err);
    return;
  }
  const dbCategories = rows.map(x => newCategory(x.id, x.url, x.name));

  fetchCategories(function(webCategories) {
    findDiff(dbCategories, webCategories);
    db.close();
  });
});
