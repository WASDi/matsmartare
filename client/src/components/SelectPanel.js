import React, { PureComponent } from 'react';
import Dropdown from './Dropdown.js';

import 'react-select/dist/react-select.css';
import './SelectPanel.css';

const filterOptions = [
    { value: 'xxx', label: 'TODO' }
];

function sortKeyObject(sortKey, reverse) {
  return {
    key,
    reverse
  }
}
const sortOptions = [
    { value: sortKeyObject('price', true), label: 'Billigast' },
    { value: sortKeyObject('discount', false), label: 'Mest rabatt' },

    { value: sortKeyObject('first_seen', true), label: 'Senast inkomna' },
    { value: sortKeyObject('best_before', false), label: 'Bäst före' },

    { value: sortKeyObject('price', false), label: 'Dyrast' },
    { value: sortKeyObject('discount', true), label: 'Minst rabatt' }
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
