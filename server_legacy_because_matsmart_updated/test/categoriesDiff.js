import request from 'request';
import cheerio from 'cheerio';
import sqlite3 from 'sqlite3';

import {
  newCategory
} from "../src/lib/models.js";
import fetchCategories from '../src/lib/fetch-categories.js';

function findAndRepairDiff(dbCategories, webCategories, db) {
  let diffFound = false;
  if (dbCategories.length !== webCategories.length) {
    console.log(" ##### DIFFERENT LENGTH ##### DB = " + dbCategories.length + ", WEB = " + webCategories.length);
    diffFound = true;
  }

  db.run("BEGIN TRANSACTION");
  let insertStmt = db.prepare("INSERT INTO categories (url, name) VALUES (?, ?)");
  let deleteStmt = db.prepare("DELETE FROM categories WHERE id = ?");

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
      deleteStmt.run(dbCat.id);
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
      insertStmt.run(webCat.url, webCat.name);
    }
  }

  insertStmt.finalize();
  deleteStmt.finalize();
  db.run("COMMIT");

  if (!diffFound) {
    console.log(" ----- no category diffs -----");
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
    findAndRepairDiff(dbCategories, webCategories, db);
    db.close();
  });
});
