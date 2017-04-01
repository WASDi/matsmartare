import React, { PureComponent } from 'react';
import Dropdown from './Dropdown.js';

import 'react-select/dist/react-select.css';
import './SelectPanel.css';

function transformCategories(categories) {
  const categoryOptions = [];
  categories.forEach(category => {
    categoryOptions.push({
      value: category.id,
      label: category.name
    });
  });
  return categoryOptions;
}

function sortKeyObject(key, descending) {
  return {
    key,
    descending
  }
}

const sortOptions = [
    { value: sortKeyObject('price', false), label: 'Billigast' },
    { value: sortKeyObject('discount', true), label: 'Mest rabatt' },

    { value: sortKeyObject('first_seen', true), label: 'Senast inkomna' },
    { value: sortKeyObject('best_before', false), label: 'Kortast datum' },

    { value: sortKeyObject('price', true), label: 'Dyrast' },
    { value: sortKeyObject('discount', false), label: 'Minst rabatt' }
];

export default class SelectPanel extends PureComponent {

  constructor (props) {
    super(props);
    this.categoryOptions = transformCategories(props.categories);
  }

  render () {
    return (
      <div>
        <Dropdown options={this.categoryOptions} placeholder="Kategorier..." align="left" onChange={this.props.onFilterChange} />
        <Dropdown options={sortOptions} placeholder="Sortera..." align="right" onChange={this.props.onSortChange} />
      </div>
    )
  }
}
