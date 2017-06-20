import React, { Component } from 'react';
import './PriceChangesPage.css';

class PriceChangesPage extends Component {

  constructor(props) {
    super(props);
    this.state = {
      priceChangeItems: props.priceChangeItems
    };
  }

  render() {
    // this.props.onMenuClick
    return (
      <div style={{color: "white"}}>
        PRICE CHANGES !!!
      </div>
    );
  }
}

export default PriceChangesPage;
