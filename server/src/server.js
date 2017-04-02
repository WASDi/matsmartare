import {
  fetchItemsFromDb
} from './lib/fetch-items-from-db.js';
import sqlite3 from 'sqlite3';
import express from 'express';

const server = express();
const PORT = 4000;

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

server.get("/everything.json", (req, res) => {
  const db = new sqlite3.Database("matsmartare.db");
  fetchItemsFromDb(db, false).then(items => {
    fetchCategoriesFromDb(db, categories => {
      res.json({
        categories,
        items
      });
      db.close();
    });
  });
});

server.use('/static', express.static(__dirname + "/client/static"));

server.get("/", (req, res) => {
  res.sendFile(__dirname + "/client/index.html");
});

server.listen(PORT);

console.log("Server running on http://localhost:" + PORT);
