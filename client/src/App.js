import React, { Component } from 'react';
import ItemList from './components/ItemList.js';
import SelectPanel from './components/SelectPanel.js';
import fetchItems from './util/ItemFetcher.js';
import filterSort from './util/ItemListFiltererSorter';
import './App.css';

class App extends Component {

  constructor() {
    super();
    this.state = {};

    this.onFilterChange = this.onFilterChange.bind(this);
    this.onSortChange = this.onSortChange.bind(this);

    fetchItems(data => {
      this.setState(
        {
          rawItems: data.items,
          items: data.items,
          categories: data.categories
        }
      );
    });
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
      <div className="App">
      { items ?
        <div>
          <div className="App-header">
            <h2>Matsmartare</h2>
            <SelectPanel categories={categories} onFilterChange={this.onFilterChange} onSortChange={this.onSortChange} />
          </div>

          <div className="App-body">
            <ItemList items={items} />
          </div>
        </div>
        :
        "Loading..."
      }
      </div>
    );
  }
}

export default App;
