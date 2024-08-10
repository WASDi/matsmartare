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
            Prisändringar
            <div className="menuButton" onClick={this.props.onMenuClick}>
              <svg width="28" height="28" fill="none" style={{verticalAlign: 'middle'}}>
                <path stroke="black" strokeLinecap="round" strokeWidth="2" d="M5 6h18M5 14h18M5 22h18M5"></path>
              </svg>
            </div>
          </div>
        </Headroom>

        <PriceChangesList priceChangeItems={this.state.priceChangeItems} />
      </div>
    );
  }
}

export default PriceChangesPage;
