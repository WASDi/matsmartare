import React, { PureComponent } from 'react';
import Dropdown from './Dropdown.js';

import 'react-select/dist/react-select.css';
import './SelectPanel.css';

const filterOptions = [
    { value: 4, label: 'TODO 4' },
    { value: 18, label: 'TODO 18' },
];

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
