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

function listsIntersect(list1, list2) {
    for (let item of list1) {
        if (list2.includes(item)) {
            return true;
        }
    }
    return false;
}

export default function filterSort(itemList, categoryId, sortKeyObject, categories) {

  if (categoryId) {
    if (categoryId === 'NO_CANDY') {
      const candyIds = categories.filter(x => {
        const name = x.name.toLowerCase()
        return (name.includes('godis') && !name.includes('naturgodis')) || name.includes('kakor')
      }).map(x => x.id)

      itemList = itemList.filter(x => !listsIntersect(x.categories, candyIds));
    } else {
      itemList = itemList.filter(x => x.categories.indexOf(categoryId) !== -1);
    }
  }

  if (sortKeyObject) {
    itemList = sortByKey(itemList, sortKeyObject);
  }

  return itemList;
}
