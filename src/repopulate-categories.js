import request from 'request';
import cheerio from 'cheerio';

const extURL = "http://www.matsmart.se/";
request(extURL, function(error, response, html) {
  if (error) {
    console.log(error);
    return;
  }
  const $ = cheerio.load(html);
  const menuItems = $("#category-menu > li > a");

  menuItems.each(function(i, element) {
    let categoryUrl = element.attribs.href;
    let categoryName = element.children[0].data;
    console.log(categoryUrl);
    console.log(categoryName);
    console.log();
  });
});
