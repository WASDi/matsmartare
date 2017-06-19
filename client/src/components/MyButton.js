import React, { PureComponent } from 'react';
import './MyButton.css';

export default class MyButon extends PureComponent {

  render () {
    return (
      <div className="myButton" onClick={this.props.onClick}>
        {this.props.children}
      </div>
    )
  }
}
