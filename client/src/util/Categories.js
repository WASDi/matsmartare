class Categories {
  constructor(categories, multisearchValue) {
    this.categories = categories;
    this.multisearchValue = multisearchValue;
  }

  toLabelValues() {
    return this.categories.map(cat => ({
      value: cat.id,
      label: cat.name
    }));
  }

  candyIds() {
    return this.categories.filter(x => {
      const name = x.name.toLowerCase()
      return (name.includes('godis') && !name.includes('naturgodis')) || name.includes('kakor')
    }).map(x => x.id)
  }

  initialFilterKey() {
    if (this.multisearchValue) {
      return 'MULTISEARCH'
    } else {
      return null;
    }
  }

  filterMultisearch(itemList) {
    const searchTerms = this.multisearchValue.split('\n').filter(line => line.trim() !== '').map(term => term.toLowerCase());
    if (searchTerms.length == 0) {
      return [];
    }

    const include = searchTerms.filter(s => !s.startsWith('-'))
    const exclude = searchTerms.filter(s => s.startsWith('-')).map((s) => s.substring(1))

    return itemList.filter(x => {
      const name = x.name.toLowerCase();
      return include.some(term => name.includes(term))
             && !exclude.some(term => name.includes(term));
    });
  }

}

export default Categories;
