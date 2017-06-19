import React, { PureComponent } from 'react';
import MyButton from './components/MyButton.js';
import './MenuPage.css';

class MenuPage extends PureComponent {

  render() {
    // TODO FIXME clicking gotoItemPage loads the AJAX again
    return (
      <div className="menuWrapper">
        <MyButton onClick={this.props.gotoItemPage}>Alla varor</MyButton>
        <MyButton onClick={this.props.gotoPriceChangesPage}>Prisändringar</MyButton>
      </div>
    );
  }
}

export default MenuPage;
