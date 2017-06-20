function createItemIdMap(items) {
  const itemIdMap = {};
  items.forEach(item => {
    itemIdMap[item.id] = item;
  });
  return itemIdMap;
}

function generatePriceChangesItems(items, priceChangesRaw) {
  const itemIdMap = createItemIdMap(items);
  const priceChangesItems = [];
  priceChangesRaw.forEach(raw => {
    priceChangesItems.push({
      itemName: itemIdMap[raw.item_id].name,
      priceBefore: raw.price_before,
      priceAfter: raw.price_after
    });
  });
  return priceChangesItems;
}

export default generatePriceChangesItems;
