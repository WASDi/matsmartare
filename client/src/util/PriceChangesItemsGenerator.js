import dateFormat from 'dateformat';
import formatPrice from './PriceFormatter.js'

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
        item: item,
        priceBefore: formatPrice(raw.price_before),
        priceAfter: formatPrice(raw.price_after),
        dateOfChange: dateFormat(raw.created * 1000, "yyyy-mm-dd"),
        becameCheaper: raw.price_after < raw.price_before
      });
    }
  });
  return priceChangeItems;
}

export default generatePriceChangeItems;
