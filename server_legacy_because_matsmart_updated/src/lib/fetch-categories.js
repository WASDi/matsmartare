import request from 'request';
import cheerio from 'cheerio';
import sqlite3 from 'sqlite3';

import { newCategory } from './models.js';

const extURL = "http://www.matsmart.se/";

export default function fetchCategories(callback) {
  request(extURL, function(error, response, html) {
    if (error) {
      console.log(error);
      return;
    }
    const $ = cheerio.load(html);
    const menuItems = $("#category-menu > li > a");

    if (!menuItems || menuItems.length == 0) {
      console.log("menuItems empty?");
      return;
    }

    let categories = [];
    menuItems.each(function(id, element) {
      let categoryUrl = trimDomain(element.attribs.href);
      let categoryName = element.children[0].data.trim();
      categories.push(newCategory(id+1, categoryUrl, categoryName));
    });
    callback(categories);
  });
}

function trimDomain(url) {
  const domain = 'matsmart.se'
  let matsmartare_idx = url.indexOf(domain);
  if (matsmartare_idx != -1) {
    return url.substr(matsmartare_idx + domain.length)
  }
  return url;
}
