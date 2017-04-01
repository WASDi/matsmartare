import React, { PureComponent } from 'react';
import Select from 'react-select';
import 'react-select/dist/react-select.css';

import './SelectPanel.css';

function logChange(val) {
    console.log("Selected: " + val.value);
}

let options = [
    { value: 'one', label: 'One' },
    { value: 'two', label: 'Two' }
];

export default class SelectPanel extends PureComponent {

  constructor (props) {
    super(props);
  }

  render () {
    return (
      <div>
        <div className="dropdown" style={{float: "left"}}>
          <Select
            name="form-field-name"
            value=""
            options={options}
            onChange={logChange}
            searchable={false}
            placeholder="Filtrera..."
          />
        </div>
        <div className="dropdown" style={{float: "right"}}>
          <Select
            name="form-field-name"
            value=""
            options={options}
            onChange={logChange}
            searchable={false}
            placeholder="Sortera..."
          />
        </div>
      </div>
    )
  }
}
