import React, { PureComponent } from 'react';
import {List, AutoSizer, WindowScroller} from 'react-virtualized';
import './PriceChangesList.css';

const ROW_HEIGHT = 80;

const imageWrapperStyle = {
  width: ROW_HEIGHT-10,
  height: ROW_HEIGHT-10
};

function openLinkInNewWindow(url) {
  window.open(url, "_blank");
}

export default class PriceChangesList extends PureComponent {

  constructor (props) {
    super(props);
    this.state = {
      items: props.priceChangeItems
    };

    this._rowRenderer = this._rowRenderer.bind(this);
  }

  render () {
    return (
      <div>
        <WindowScroller>
        {({ height, scrollTop }) => (
          <AutoSizer disableHeight>
              {({ width }) => (
                <List
                  autoHeight
                  ref={listRef => { this.listRef = listRef }}
                  overscanRowCount={5}
                  rowCount={this.state.items.length}
                  rowHeight={ROW_HEIGHT}
                  rowRenderer={this._rowRenderer}
                  width={width}
                  height={height}
                  scrollTop={scrollTop}
                />
              )}
            </AutoSizer>
          )}
          </WindowScroller>
      </div>
    )
  }

  _rowRenderer ({ index, key, style }) {
      const element = this.state.items[index];

      const imageStyle = {
        width: ROW_HEIGHT-10,
        height: ROW_HEIGHT-15,
        backgroundImage : 'url(' + element.item.img_url + ')'
      };

      return (
        <div className="itemRow" key={key} style={style}>

          <div className="itemImageWrapper" style={imageWrapperStyle} onClick={() => openLinkInNewWindow(element.item.url)}>
            <div className="itemImage" style={imageStyle}></div>
          </div>

          <div className="price">
            <div className="priceBefore">{element.priceBefore}</div>
            <div className="priceAfter">{element.priceAfter}</div>
          </div>

          <div className="itemInfoWrapper">
            <div className="itemName">
              <a href={element.item.url} target="_blank">{element.item.name}</a>
            </div>
            <div className="itemInfo">
              {element.becameCheaper ? <span className="cheaper">▼</span> : <span className="expensiver">▲</span>}
              {element.dateOfChange}
            </div>
          </div>

        </div>
      )
    }

}
