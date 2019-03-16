import request from 'request';
import cheerio from 'cheerio';
import sqlite3 from 'sqlite3';
import fetchCategories from './lib/fetch-categories.js';

fetchCategories(function(categories) {
  let db = new sqlite3.Database("matsmartare.db");
  db.run("BEGIN TRANSACTION");
  db.run("DELETE FROM categories");
  categories.forEach(function(category) {
    db.run("INSERT INTO categories (id, url, name) VALUES (?,?,?)", category.id, category.url, category.name);
  });
  db.run("COMMIT");
  db.close();
});
