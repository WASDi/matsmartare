import {
  fetchItemsFromDb,
  resolveCategories
} from './fetch-items-from-db.js';
import sqlite3 from 'sqlite3';
import express from 'express';

const server = express();

server.get("/db.json", (req, res) => {
  const db = new sqlite3.Database("matsmartare.db");
  fetchItemsFromDb(db, false).then(values => {
    res.json(values);
    db.close();
  });
});

server.get("/categories.json", (req, res) => {
  const db = new sqlite3.Database("matsmartare.db");
  db.all("SELECT id, url, title FROM categories", function(err, rows) {
    const categories = resolveCategories(rows);
    res.json(categories);
    db.close();
  });
});

server.get("/", (req, res) => {
  res.sendFile(__dirname + "/index.html");
});

server.listen(4000);

console.log("Server started");
