import React, { Component } from 'react';
import Headroom from 'react-headroom';

import './PriceChangesPage.css';
import PriceChangesList from './components/PriceChangesList.js';


class PriceChangesPage extends Component {

  constructor(props) {
    super(props);
    this.state = {
      priceChangeItems: props.priceChangeItems
    };
  }

  render() {
    return (
      <div className="PriceChangesPage">
        <Headroom>
          <div className="menuHeader">
            Prisändringar <span className="menuButton" onClick={this.props.onMenuClick}>☰</span>
          </div>
        </Headroom>

        <PriceChangesList priceChangeItems={this.state.priceChangeItems} />
      </div>
    );
  }
}

export default PriceChangesPage;
