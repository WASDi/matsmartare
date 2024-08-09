import React, { Component } from 'react';
import ItemList from './components/ItemList.js';
import SelectPanel from './components/SelectPanel.js';
import Headroom from 'react-headroom';
import filterSort from './util/ItemListFiltererSorter';
import './ItemPage.css';

const INITIAL_SORT = {key: 'first_seen', descending: true};

class ItemPage extends Component {

  constructor(props) {
    super(props);
    this.state = {
      rawItems: props.items,
      items: props.items,
      categories: props.categories
    };

    filterSort(this.state.rawItems, null, INITIAL_SORT, this.state.categories);

    this.onFilterChange = this.onFilterChange.bind(this);
    this.onSortChange = this.onSortChange.bind(this);
  }

  onFilterChange(categoryFilter) {
    const newItems = filterSort(this.state.rawItems, categoryFilter, this.state.sortOptions, this.state.categories);
    this.setState({
      categoryFilter: categoryFilter,
      items: newItems
    });
  }

  onSortChange(sortOptions) {
    const newItems = filterSort(this.state.rawItems, this.state.categoryFilter, sortOptions, this.state.categories);
    this.setState({
      sortOptions: sortOptions,
      items: newItems
    });
  }

  render() {
    const { items, categories } = this.state;
    return (
      <div className="ItemPage">
        <Headroom>
          <div className="ItemPage-header">
            <div className="ItemPage-title">
              Matsmartare
              <div className="menuButton" onClick={this.props.onMenuClick}>
                <svg width="28" height="28" fill="none" style={{verticalAlign: 'middle'}}>
                  <path stroke="black" strokeLinecap="round" strokeWidth="2" d="M5 6h18M5 14h18M5 22h18M5"></path>
                </svg>
              </div>
              <div className="menuButton">
                <svg width="28" height="28" fill="none" style={{verticalAlign: 'middle'}}>
                  <path d="m17 17 6 6" stroke="black" strokeWidth="2" strokeLinecap="round"></path>
                  <circle cx="12" cy="12" r="7" stroke="black" strokeWidth="2"></circle>
                </svg>
              </div>
            </div>
            <div className="ItemPage-subtitle">Ett smidigare gränssnitt till matsmart.se</div>
            <SelectPanel categories={categories} onFilterChange={this.onFilterChange} onSortChange={this.onSortChange} />
          </div>
        </Headroom>

        <div className="ItemPage-body">
          <ItemList items={items} />
        </div>
      </div>
    );
  }
}

export default ItemPage;
