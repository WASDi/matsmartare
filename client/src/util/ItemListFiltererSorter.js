function sortByKey(array, sortKeyObject) {
  const {
    key,
    reverse
  } = sortKeyObject;
  return array.sort(function(a, b) {
    const x = a[key];
    const y = b[key];
    if (reverse) {
      return ((x > y) ? -1 : ((x < y) ? 1 : 0));
    } else {
      return ((x < y) ? -1 : ((x > y) ? 1 : 0));
    }
  });
}

export default function(itemList, categoryKey, sortKeyObject) {

  if (categoryKey !== undefined) {
    itemList = itemList.filter(x => x.categories.contains(categoryKey));
  }

  if (sortKeyObject !== undefined) {
    itemList = sortByKey(itemList, sortKeyObject);
  }

  return itemList;
}
