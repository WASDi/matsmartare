import React, { PureComponent } from 'react';
import './MultisearchPage.css';

class MultisearchPage extends PureComponent {

  constructor() {
    super();
    this.textareaRef = React.createRef();
    this.onSearch = this.onSearch.bind(this);
  }

  onSearch() {
    const searchText = this.textareaRef.current.value;
    this.props.onMultisearch(searchText);
//    const searchTerms = searchText.split('\n').filter(line => line.trim() !== '');
//
//    if (searchTerms.length == 0) {
//      this.props.onMultisearch(null);
//    } else {
//      this.props.onMultisearch({
//        include: searchTerms.filter((s) => !s.startsWith('-')),
//        exclude: searchTerms.filter((s) => s.startsWith('-')).map((s) => s.substring(1))
//      });
//    }
  }

  render() {
    return (
      <div className="MultisearchPage">
        <div className="MultisearchPage-title">Multisök</div>
        <div>
          Ett sökord per rad.<br/>
          Minustecken framför för att exkludera.
        </div>
        <textarea ref={this.textareaRef}></textarea>
        <div className="MultisearchPage-button" onClick={this.onSearch}>Sök</div>
      </div>
    );
  }
}

export default MultisearchPage;
