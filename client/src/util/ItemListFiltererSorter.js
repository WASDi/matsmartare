function sortByKey(array, sortKeyObject) {
  const {
    key,
    descending
  } = sortKeyObject;
  return array.sort(function(a, b) {
    const x = a[key];
    const y = b[key];

    if (x != null && y == null) {
      return descending ? 1 : -1;
    } else if (x == null && y != null) {
      return descending ? -1 : 1;
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
    if (categoryKey === window.NO_CANDY) {
      itemList = itemList.filter(x => x.categories.indexOf(window.CANDY_ID) === -1);
    } else {
      itemList = itemList.filter(x => x.categories.indexOf(categoryKey) !== -1);
    }
  }

  if (sortKeyObject) {
    itemList = sortByKey(itemList, sortKeyObject);
  }

  return itemList;
}
