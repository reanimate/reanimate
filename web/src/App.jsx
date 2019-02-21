import React, {Component} from 'react';
import './App.css';
import AceEditor from 'react-ace';
import brace from 'brace';
import 'brace/mode/haskell';
import 'brace/theme/github';
import 'brace/theme/monokai';

class App extends Component {
  render() {
    return (
      <div className="App">
        <div>
          <select>
            <optgroup label="Examples">
              <option value="">Drawing LaTeX</option>
              <option value="">Colorful LaTeX</option>
            </optgroup>
            <optgroup label="Demos">
              <option value="">Bounding Boxes</option>
            </optgroup>
            <optgroup label="API">
              <option value="">LaTeX Basic</option>
            </optgroup>
          </select>
        </div>
        <div>
          <AceEditor
            mode="haskell"
            width="100%"
            height="100%"
            theme="monokai"
            name="UNIQUE_ID_OF_DIV"
            editorProps={{
              $blockScrolling: true
            }}/>
        </div>
        <div>Controls</div>
        <div>
          Viewer
          <div className="messages">
            <p>Compilation successful.</p>
          </div>
        </div>
      </div>
    );
  }
}

export default App;
