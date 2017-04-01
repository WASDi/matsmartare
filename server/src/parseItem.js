import { newItem } from './models.js';

export default function parseItem($, element, categoryId) {
  const itemURL = element.attribs.href;

  const imgElement = $(element).find("img.zoom").first()[0];
  const itemImageURL = imgElement.attribs.src.replace(/\?itok=.*/g, '');
  let bestBefore = undefined;
  const bestBeforeMatch = imgElement.attribs.title.match(/\d\d\d\d-\d\d-\d\d/);
  if (bestBeforeMatch != undefined) {
    bestBefore = bestBeforeMatch[0];
  }

  const name = $(element).find("span.prd-name").first().text().replace(/^\s+|\s+$/g, '');
  const discount = $(element).find("span.prd-discount-oldprice > span").first().text().replace(/^[^\(]+..|..[^\)]+$/g, '');

  let price = $(element).find("div.prd-price-num").first().text().match(/\d+/)[0];
  const itemsForPriceElem = $(element).find("span.prd-mp-text").first();
  if (itemsForPriceElem.length != 0) {
    const itemsForPrice = itemsForPriceElem.text().match(/\d+/)[0];
    price = price / itemsForPrice;
  }

  return newItem(-1, [categoryId], itemURL, itemImageURL, name, price, discount, bestBefore, 0, 0);
}
