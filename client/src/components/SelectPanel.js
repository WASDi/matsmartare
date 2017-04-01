import React, { PureComponent } from 'react';
import Dropdown from './Dropdown.js';

import 'react-select/dist/react-select.css';
import './SelectPanel.css';

const filterOptions = [
    { value: 'xxx', label: 'TODO' }
];


function sortKey(key, reverse) {
  return {
    key,
    reverse
  }
}
const sortOptions = [
    { value: sortKey('price', true), label: 'Billigast' },
    { value: sortKey('discount', false), label: 'Mest rabatt' },

    { value: sortKey('first_seen', true), label: 'Senast inkomna' },
    { value: sortKey('best_before', false), label: 'Bäst före' },

    { value: sortKey('price', false), label: 'Dyrast' },
    { value: sortKey('discount', true), label: 'Minst rabatt' }
];

export default class SelectPanel extends PureComponent {

  // constructor (props) {
  //   super(props);
  // }

  render () {
    return (
      <div>
        <Dropdown options={filterOptions} placeholder="Filtrera..." align="left" onChange={this.props.onFilterChange} />
        <Dropdown options={sortOptions} placeholder="Sortera..." align="right" onChange={this.props.onSortChange} />
      </div>
    )
  }
}
