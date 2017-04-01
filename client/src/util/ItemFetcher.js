import dateFormat from 'dateformat';

function formatPrice(price) {
  if (typeof(price) === 'number' && price % 1 !== 0) {
    return price.toFixed(2);
  }
  return price + ":-";
}

function fetchItems(callback) {
  fetch("everything.json")
    .then(resp => resp.json())
    .then(data => {
      let transformedData = []
      data.items.forEach(element => {
        const firstSeenDate = new Date(element.first_seen * 1000);
        transformedData.push({
          id: element.id,
          categories: element.categories,
          url: "http://www.matsmart.se" + element.url,
          img_url: element.img_url,
          name: element.name,
          price: element.price,
          priceFormatted: formatPrice(element.price),
          discount: element.discount,
          best_before: element.best_before ? element.best_before : null,
          first_seen: dateFormat(firstSeenDate, "yyyy-mm-dd HH:MM")
        });
      });
      callback(transformedData);
    });
}

export default fetchItems;
