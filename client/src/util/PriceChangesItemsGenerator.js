function createItemIdMap(items) {
  const itemIdMap = {};
  items.forEach(item => {
    itemIdMap[item.id] = item;
  });
  return itemIdMap;
}

function generatePriceChangeItems(items, priceChangesRaw) {
  const itemIdMap = createItemIdMap(items);
  const priceChangeItems = [];
  priceChangesRaw.forEach(raw => {
    priceChangeItems.push({
      itemName: itemIdMap[raw.item_id].name,
      priceBefore: raw.price_before,
      priceAfter: raw.price_after
    });
  });
  return priceChangeItems;
}

export default generatePriceChangeItems;
