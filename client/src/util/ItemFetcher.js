import dateFormat from 'dateformat';
import formatPrice from './PriceFormatter.js'

const TIMESTAMP_NOW = Math.round(new Date().getTime() / 1000);

function fetchItems(callback) {
  fetch("everything.json")
    .then(resp => resp.json())
    .then(data => {
      let transformedItems = []
      data.items.forEach(element => {
        const firstSeenDate = new Date(element.first_seen * 1000);
        const hoursOld = Math.round((TIMESTAMP_NOW-element.first_seen)/3600);
        transformedItems.push({
          id: element.id,
          categories: element.categories,
          url: "http://www.matsmart.se" + element.url,
          img_url: element.img_url,
          name: element.name,
          price: element.price,
          priceFormatted: formatPrice(element.price),
          discount: element.discount,
          best_before: element.best_before ? element.best_before : null,
          first_seen: dateFormat(firstSeenDate, "yyyy-mm-dd HH:MM"),
          not_waste: element.not_waste,
          hoursOld: hoursOld
        });
      });
      callback({
        items: transformedItems,
        categories: data.categories,
        priceChanges: data.priceChanges,
      });
    });
}

export default fetchItems;
