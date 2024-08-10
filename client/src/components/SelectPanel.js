import React, { PureComponent } from 'react';
import Dropdown from './Dropdown.js';

import './SelectPanel.css';

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
    this.categoryOptions = props.categories.toLabelValues();
    this.preSelect = !!props.categories.multisearchValue;
  }

  render () {
    return (
      <div>
        <Dropdown onChange={this.props.onFilterChange} options={this.categoryOptions} preSelect={this.preSelect} align="left" placeholder="Kategorier..." />
        <Dropdown onChange={this.props.onSortChange} options={sortOptions} mandatory align="right" />
      </div>
    )
  }
}
