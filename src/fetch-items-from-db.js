import HashMap from 'hashmap';
import { newItem } from './models.js';

function translateToMap(dbItems) {
  let map = new HashMap();
  dbItems.forEach(function(item) {
    map.set(item.url, item);
  });
  return map;
}

export default function fetchItemsFromDb(db, asMap) {
  return new Promise((resolve, reject) => {
    db.all("SELECT * FROM items", function(err, rows) {
      if (err) {
        reject(err);
      }
      let items = [];
      rows.forEach(function(row) {
        let item = newItem(row.id, row.categories.split(",").map(Number), row.url, row.img_url, row.name, row.price, row.discount, row.first_seen, row.last_seen);
        items.push(item);
      });
      if (asMap) {
        items = translateToMap(items);
      }
      resolve(items);
    });
  });
};
