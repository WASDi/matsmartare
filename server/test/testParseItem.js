import request from 'request';
import cheerio from 'cheerio';
import parseItem from '../src/parseItem.js';

const URL = "http://www.matsmart.se/apotek";

request(URL, function(error, response, html) {
  if (error) {
    reject(error);
    return;
  }
  const $ = cheerio.load(html);
  const products = $("div.prd > a");
  let items = [];
  products.each(function(id, element) {
    let item = parseItem($, element, 555);
    console.log(item);
  });
});
