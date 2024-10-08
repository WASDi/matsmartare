import React, { PureComponent } from 'react';
import {List, AutoSizer, WindowScroller} from 'react-virtualized';
import './ItemList.css';

const MAX_HOURS_OLD_FOR_NEW = 48;
const ROW_HEIGHT = 80;

const imageWrapperStyle = {
  width: ROW_HEIGHT-10,
  height: ROW_HEIGHT-10
};

function openLinkInNewWindow(url) {
  window.open(url, "_blank");
}

export default class ItemList extends PureComponent {

  constructor (props) {
    super(props);
    this._rowRenderer = this._rowRenderer.bind(this);
  }

  componentDidUpdate(prevProps) {
    if (prevProps.items !== this.props.items) {
      this.listRef.forceUpdateGrid();
    }
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
                  rowCount={this.props.items.length}
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
      const element = this.props.items[index];

      const imageStyle = {
        width: ROW_HEIGHT-10,
        height: ROW_HEIGHT-15,
        backgroundImage : 'url(' + element.img_url + ')'
      };

      return (
        <div className="itemRow" key={key} style={style}>

          <div className="itemImageWrapper" style={imageWrapperStyle} onClick={() => openLinkInNewWindow(element.url)}>
            <div className="itemImage" style={imageStyle}></div>
          </div>

          <div className="itemPrice">
            <b>{element.priceFormatted}</b>
          </div>

          <div className="itemInfoWrapper">
            <div className="itemName">
              <a href={element.url} target="_blank">{element.name}</a>
              {element.not_waste ? <span title="Inte matsvinn!"> ⚠️</span> : ""}
            </div>
            <div className="itemInfo">
              <div className="itemDiscount">-{element.discount}%</div>
              {element.hoursOld <= MAX_HOURS_OLD_FOR_NEW ? <div className="itemNyhet">{element.hoursOld}h</div> : ""}
              {element.best_before ? <div className="itemBestBefore">Bäst före {element.best_before}</div> : ""}
            </div>
          </div>

        </div>
      )
    }

}
