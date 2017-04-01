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

export default function filterSort(itemList, categoryKey, sortKeyObject) {

  if (categoryKey) {
    itemList = itemList.filter(x => x.categories.indexOf(categoryKey) !== -1);
  }

  if (sortKeyObject) {
    itemList = sortByKey(itemList, sortKeyObject);
  }

  return itemList;
}
