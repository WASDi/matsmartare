import dateFormat from 'dateformat';

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
    const item = itemIdMap[raw.item_id];
    if (item) {
      // Should always be true because server is supposed to filter out price changes for removed items, but just to be safe.
      priceChangeItems.push({
        itemName: item.name,
        priceBefore: raw.price_before,
        priceAfter: raw.price_after,
        dateOfChange: dateFormat(raw.created * 1000, "yyyy-mm-dd")
      });
    }
  });
  return priceChangeItems;
}

export default generatePriceChangeItems;
