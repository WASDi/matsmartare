function newItem(id, categories, url, img_url, name, price, discount, first_seen, last_seen) {
  return {
    id,
    categories,
    url,
    img_url,
    name,
    price,
    discount,
    first_seen,
    last_seen
  };
}

function newCategory(id, url, title) {
  return {
    id,
    url,
    title
  };
}

module.exports = {
  newItem,
  newCategory
}
