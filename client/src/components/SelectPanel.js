import React, { PureComponent } from 'react';
import Dropdown from './Dropdown.js';

import 'react-select/dist/react-select.css';
import './SelectPanel.css';

function transformCategories(categories) {
  categories.sort(function(a,b) {return a.name > b.name} );
  const categoryOptions = categories.map(cat => ({value: cat.id, label: cat.name}));
  categoryOptions.unshift({
    value: window.NO_CANDY,
    label: '[Allt förutom godis]'
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
  { value: sortKeyObject('first_seen', true), label: 'Senast inkomna' },

  { value: sortKeyObject('price', false), label: 'Lägst pris' },
  { value: sortKeyObject('price', true), label: 'Högst pris' },

  { value: sortKeyObject('discount', true), label: 'Mest rabatt' },
  { value: sortKeyObject('discount', false), label: 'Minst rabatt' },

  { value: sortKeyObject('best_before', false), label: 'Kortast datum' },
  { value: sortKeyObject('best_before', true), label: 'Längst datum' }
];

export default class SelectPanel extends PureComponent {

  constructor (props) {
    super(props);
    this.categoryOptions = transformCategories(props.categories);
  }

  render () {
    return (
      <div>
        <Dropdown onChange={this.props.onFilterChange} options={this.categoryOptions} align="left" placeholder="Kategorier..." />
        <Dropdown onChange={this.props.onSortChange} options={sortOptions} mandatory align="right" />
      </div>
    )
  }
}
