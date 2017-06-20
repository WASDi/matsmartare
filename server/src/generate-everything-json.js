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

function fetchPriceChangesFromDb(db, callback) {
  db.all("SELECT item_id, price_before, price_after, created FROM price_changes ORDER BY created DESC", function(err, rows) {
    const priceChanegs = [];
    rows.forEach(row => {
      priceChanegs.push({
        item_id: row.item_id,
        price_before: row.price_before,
        price_after: row.price_after,
        created: row.created
      });
    });
    callback(priceChanegs);
  });
}

const db = new sqlite3.Database("matsmartare.db");
fetchItemsFromDb(db, false).then(items => {
  fetchCategoriesFromDb(db, categories => {
    fetchPriceChangesFromDb(db, priceChanges => {
      const everything = ({
        categories,
        items,
        priceChanges
      });
      db.close();
      writeToFile(everything);
    });
  });
});
