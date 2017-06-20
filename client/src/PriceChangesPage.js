import React, { Component } from 'react';
import './PriceChangesPage.css';

class PriceChangesPage extends Component {

  constructor(props) {
    super(props);
    this.state = {
      priceChangesItems: props.priceChangesItems
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
