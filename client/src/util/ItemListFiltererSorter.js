function sortByKey(array, sortKeyObject) {
  const {
    key,
    descending
  } = sortKeyObject;
  return array.sort(function(a, b) {
    const x = a[key];
    const y = b[key];
    if (y == null) {
      return -1;
    }
    const retVal = ((x < y) ? -1 : ((x > y) ? 1 : 0));
    if (descending) {
      return -retVal;
    }
    return retVal;
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
