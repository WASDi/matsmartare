import React, { PureComponent } from 'react';
import Select from 'react-select';

export default class Dropdown extends PureComponent {

  constructor (props) {
    super(props);
    this.state = {
      selectOption: props.mandatory ? props.options[0] : null
    };
    this.updateValue = this.updateValue.bind(this);
  }

  updateValue (newOption) {
    this.setState({
      selectOption: newOption
    });
    this.props.onChange(newOption ? newOption.value : null);
  }

  render () {
    const {placeholder, align, options, mandatory} = this.props;
    const customStyles = {
      option: (provided, state) => ({
        ...provided,
        color: state.isFocused ? '#333' : '#666',
      }),
    };
    return (
      <div className="dropdown" style={{float: align}}>
        <Select
          value={this.state.selectOption}
          onChange={this.updateValue}
          options={options}
          searchable={false}
          placeholder={placeholder}
          clearable={!mandatory}
          styles={customStyles}
        />
      </div>
    )
  }
}
