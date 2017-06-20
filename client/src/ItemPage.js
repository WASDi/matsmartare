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

    filterSort(this.state.rawItems, null, INITIAL_SORT);

    this.onFilterChange = this.onFilterChange.bind(this);
    this.onSortChange = this.onSortChange.bind(this);
  }

  onFilterChange(categoryFilter) {
    const newItems = filterSort(this.state.rawItems, categoryFilter, this.state.sortOptions);
    this.setState({
      categoryFilter: categoryFilter,
      items: newItems
    });
  }

  onSortChange(sortOptions) {
    const newItems = filterSort(this.state.rawItems, this.state.categoryFilter, sortOptions);
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
            <div className="ItemPage-title">Matsmartare <span className="menuButton" onClick={this.props.onMenuClick}>☰</span></div>
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
