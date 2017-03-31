import React, { PureComponent } from 'react';
import {List, AutoSizer} from 'react-virtualized';
import './ItemList.css';

export default class MyVirtualList extends PureComponent {

  constructor (props) {
    super(props);
    this.state = {
      items: props.items
    };

    this._rowRenderer = this._rowRenderer.bind(this);
  }

  render () {
    return (
      <div>
        <AutoSizer disableHeight>
            {({ width }) => (
              <List
                ref='List'
                overscanRowCount={10}
                rowCount={this.state.items.length}
                rowHeight={80}
                rowRenderer={this._rowRenderer}
                width={width}
                height={500}
              />
            )}
          </AutoSizer>
      </div>
    )
  }

  _rowRenderer ({ index, key, style }) {
      const element = this.state.items[index];
      return (
        <div className="itemRow" key={key} style={style}>

          <div className="itemPrice">
            <b>{element.priceFormatted}</b>
          </div>

          <div>
            <div className="itemName">
              <a href={element.url} target="_blank">{element.name}</a>
            </div>
            <div className="itemInfo">
              <small>-{element.discount}%{element.best_before ? " Bäst före " + element.best_before : ""}</small>
            </div>
          </div>

        </div>
      )
    }

}
