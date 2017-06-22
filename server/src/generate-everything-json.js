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
  db.all("SELECT pc.item_id, pc.price_before, pc.price_after, pc.created FROM price_changes pc JOIN items i ON pc.item_id = i.id WHERE i.last_seen=(SELECT MAX(last_seen) FROM items) ORDER BY pc.created DESC", function(err, rows) {
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
