import React, { PureComponent } from 'react';
import Select from 'react-select';

export default class Dropdown extends PureComponent {

  constructor (props) {
    super(props);
    this.state = {};
    this.updateValue = this.updateValue.bind(this);
  }

  updateValue (newOption) {
		this.setState({
			selectOption: newOption
		});
    this.props.onChange(newOption ? newOption.value : null);
	}

  render () {
    const {placeholder, align, options} = this.props;
    return (
      <div className="dropdown" style={{float: align}}>
        <Select
          value={this.state.selectOption}
          onChange={this.updateValue}
          options={options}
          searchable={false}
          placeholder={placeholder}
        />
      </div>
    )
  }
}
