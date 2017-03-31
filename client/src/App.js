import React, { Component } from 'react';
import logo from './logo.svg';
import ItemList from './components/ItemList.js';
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
            <img src={logo} className="App-logo" alt="logo" />
            <h2>Matsmartare</h2>
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
