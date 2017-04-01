import React, { PureComponent } from 'react';
import Dropdown from './Dropdown.js';

import 'react-select/dist/react-select.css';
import './SelectPanel.css';

const filterOptions = [
    { value: 'xxx', label: 'TODO' }
];

const sortOptions = [
    { value: 'xxx', label: 'Billigast' },
    { value: 'xxx', label: 'Mest rabatt' },

    { value: 'xxx', label: 'Senast inkomna' },
    { value: 'xxx', label: 'Bäst före' },

    { value: 'xxx', label: 'Dyrast' },
    { value: 'xxx', label: 'Minst rabatt' }
];

export default class SelectPanel extends PureComponent {

  // constructor (props) {
  //   super(props);
  // }

  render () {
    return (
      <div>
        <Dropdown options={filterOptions} placeholder="Filtrera..." align="left" />
        <Dropdown options={sortOptions} placeholder="Sortera..." align="right" />
      </div>
    )
  }
}
