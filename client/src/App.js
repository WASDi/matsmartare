import React, { Component } from 'react';
import ItemPage from './ItemPage.js';
import MenuPage from './MenuPage.js';

const ITEM_PAGE = 1;
const MENU_PAGE = 2;
const PRICE_CHANGES_PAGE = 3;

class App extends Component {

  constructor() {
    super();
    this.state = {
      page: ITEM_PAGE
    };

    this.gotoItemPage = this.gotoItemPage.bind(this);
    this.gotoMenuPage = this.gotoMenuPage.bind(this);
    this.gotoPriceChangesPage = this.gotoPriceChangesPage.bind(this);
  }

  gotoItemPage() {
    this.setState({ page: ITEM_PAGE });
  }

  gotoMenuPage() {
    this.setState({ page: MENU_PAGE });
  }

  gotoPriceChangesPage() {
    this.setState({ page: PRICE_CHANGES_PAGE });
  }

  render() {
    switch (this.state.page) {
      case ITEM_PAGE:
        return <ItemPage onMenuClick={this.gotoMenuPage} />;
      case MENU_PAGE:
        return <MenuPage gotoItemPage={this.gotoItemPage} gotoPriceChangesPage={this.gotoPriceChangesPage} />;
      case PRICE_CHANGES_PAGE:
        return <div style={{color: "white"}}>TODO price changes</div>;
      default:
        return <div style={{color: "white"}}>ILLEGAL STATE !!!</div>;
    }
  }

}

export default App;
