function newItem(id, categories, url, img_url, name, price, discount, best_before, first_seen, last_seen) {
  return {
    id,
    categories,
    url,
    img_url,
    name,
    price,
    discount,
    best_before,
    first_seen,
    last_seen
  };
}

function newCategory(id, url, name) {
  return {
    id,
    url,
    name
  };
}

module.exports = {
  newItem,
  newCategory
}
