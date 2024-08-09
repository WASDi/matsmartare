import React, { Component } from 'react';
import fetchItems from './util/ItemFetcher.js';
import generatePriceChangeItems from './util/PriceChangesItemsGenerator.js';
import ItemPage from './ItemPage.js';
import MenuPage from './MenuPage.js';
import PriceChangesPage from './PriceChangesPage.js';
import MultisearchPage from './MultisearchPage.js';

const LOADING_PAGE = 0;
const ITEM_PAGE = 1;
const MENU_PAGE = 2;
const PRICE_CHANGES_PAGE = 3;
const MULTISEARCH_PAGE = 4;

class App extends Component {

  constructor() {
    super();
    this.state = {
      page: LOADING_PAGE
    };

    this.gotoItemPage = this.gotoItemPage.bind(this);
    this.gotoMenuPage = this.gotoMenuPage.bind(this);
    this.gotoPriceChangesPage = this.gotoPriceChangesPage.bind(this);
    this.gotoMultisearchPage = this.gotoMultisearchPage.bind(this);
    this.onMultisearch = this.onMultisearch.bind(this);

    fetchItems(data => {
      this.setState(
        {
          page: ITEM_PAGE,
          items: data.items,
          categories: data.categories,
          priceChangeItems: generatePriceChangeItems(data.items, data.priceChanges)
        }
      );
    });
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

  gotoMultisearchPage() {
    this.setState({ page: MULTISEARCH_PAGE });
  }

  onMultisearch(searchTerms) {
    console.log("onMultiSearch", searchTerms) // TODO add multisearch logic
    this.setState({ page: ITEM_PAGE });
  }

  render() {
    const { page, items, categories, priceChangeItems } = this.state;

    switch (page) {
      case LOADING_PAGE:
        return <div style={{color: "white"}}>Loading...</div>;
      case ITEM_PAGE:
        return <ItemPage onMenuClick={this.gotoMenuPage} onMultisearchClick={this.gotoMultisearchPage}
                  items={items} categories={categories} />;
      case MENU_PAGE:
        return <MenuPage gotoItemPage={this.gotoItemPage} gotoPriceChangesPage={this.gotoPriceChangesPage} />;
      case PRICE_CHANGES_PAGE:
        return <PriceChangesPage onMenuClick={this.gotoMenuPage} priceChangeItems={priceChangeItems} />;
      case MULTISEARCH_PAGE:
        return <MultisearchPage onMultisearch={this.onMultisearch} />;
      default:
        return <div style={{color: "white"}}>ILLEGAL STATE !!!</div>;
    }
  }

}

export default App;
