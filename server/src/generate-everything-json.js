import {
  fetchItemsFromDb
} from './lib/fetch-items-from-db.js';
import sqlite3 from 'sqlite3';
import fs from 'fs';

function writeToFile(everything) {
  fs.writeFile('everything.json', JSON.stringify(everything), 'utf8', () => {console.log("OK")});
}

function fetchCategoriesFromDb(db, callback) {
  db.all("SELECT id, name FROM categories", function(err, rows) {
    const categories = [];
    rows.forEach(row => {
      categories.push({
        id: row.id,
        name: row.name
      });
    });
    callback(categories);
  });
}

const db = new sqlite3.Database("matsmartare.db");
fetchItemsFromDb(db, false).then(items => {
  fetchCategoriesFromDb(db, categories => {
    const everything = ({
      categories,
      items
    });
    db.close();
    writeToFile(everything);
  });
});
