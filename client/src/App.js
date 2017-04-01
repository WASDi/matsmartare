import React, { Component } from 'react';
import ItemList from './components/ItemList.js';
import SelectPanel from './components/SelectPanel.js';
import fetchItems from './util/ItemFetcher.js';
import './App.css';

class App extends Component {

  constructor() {
    super();
    this.state = {};
    fetchItems(items => {
      this.setState({items: items});
    })
  }

  render() {
    const { items } = this.state;
    return (
      <div className="App">
      { items ?
        <div>
          <div className="App-header">
            <h2>Matsmartare</h2>
            <SelectPanel />
          </div>

          <div className="App-body">
            <ItemList items={items} / >
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
