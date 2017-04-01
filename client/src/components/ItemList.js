import React, { PureComponent } from 'react';
import {List, AutoSizer} from 'react-virtualized';
import './ItemList.css';

const ROW_HEIGHT = 80;

const imageWrapperStyle = {
  width: ROW_HEIGHT,
  height: ROW_HEIGHT-20
};

export default class ItemList extends PureComponent {

  constructor (props) {
    super(props);
    this.state = {
      items: props.items
    };

    this._rowRenderer = this._rowRenderer.bind(this);
  }

  componentWillReceiveProps(nextProps) {
    this.setState({
      items: nextProps.items
    });
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
                rowHeight={ROW_HEIGHT}
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

      const imageStyle = {
        width: ROW_HEIGHT-10,
        height: ROW_HEIGHT-15,
        backgroundImage : 'url(' + element.img_url + ')'
      };

      return (
        <div className="itemRow" key={key} style={style}>

          <div className="itemImageWrapper" style={imageWrapperStyle}>
            <div className="itemImage" style={imageStyle}></div>
          </div>

          <div className="itemPrice">
            <b>{element.priceFormatted}</b>
          </div>

          <div className="itemInfoWrapper">
            <div className="itemName">
              <a href={element.url} target="_blank">{element.name}</a>
            </div>
            <div className="itemInfo">
              <div className="itemDiscount">-{element.discount}%</div>
              {element.best_before ? <div className="itemBestBefore">Bäst före {element.best_before}</div>: ""}
            </div>
          </div>

        </div>
      )
    }

}
