import React, { Component } from 'react';

class PriceChangesPage extends Component {

  constructor(props) {
    super(props);
    this.state = {
      itemIdMap: props.itemIdMap,
      priceChanges: props.priceChanges
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
