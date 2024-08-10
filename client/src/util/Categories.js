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

}

export default Categories;
