import React, {Component} from 'react';
import './App.css';
import AceEditor from 'react-ace';
import 'brace/mode/haskell';
import 'brace/theme/github';
import 'brace/theme/monokai';

import preset from './Presets';

class App extends Component {
  connect = () => {
    const ws = new WebSocket("ws://localhost:9160");

    ws.onopen = event => {
      this.setState(state => ({...state, message: "Connected."}));
      ws.send(this.state.program);
    }
    ws.onclose = event => {
      this.setState(state => ({...state, message: "Disconnected."}));
      setTimeout(this.connect, 1000);
    }
    ws.onmessage = event => {
      if( event.data === "Success!" ) {
        console.log("Success");
      } else if( event.data === "Rendering" ) {
        this.setState({message: "Rendering..."});
        this.nFrames_new = 0;
        this.svgs_new = [];
      } else if( event.data === "Done" ) {
        this.setState({message: "Success!"});
        console.log("Done");
        this.nFrames = this.nFrames_new;
        this.svgs = this.svgs_new;
        this.nFrames_new = 0;
        this.svgs_new = [];
        this.start = Date.now();
      } else if( event.data.startsWith("Error") ) {
        console.log("Error");
        this.setState({message: event.data.substring(5) });
      } else {
        this.setState({message: `Rendering: ${this.nFrames_new}`});
        this.nFrames_new++;
        const div = document.createElement('div');
        div.innerHTML = event.data;
        this.svgs_new.push(div);
      }
    }
    this.setState(state => ({...state, socket: ws, message: "Connecting..."}));
  }
  constructor(props) {
    super(props);

    this.state = {
      program: preset[0].programs[0].code
    };
    setTimeout(this.connect,0);
    this.nFrames_new = 0;
    this.svgs_new = [];
    this.nFrames = 0;
    this.svgs = [];
    this.start = Date.now();
    const self = this;
    const animate = () => {
      const now = Date.now();
      const nFrames = self.nFrames;
      const thisFrame = (Math.round((now-this.start)/1000*60))%nFrames
      // const thisFrame = 0;
      // console.log('Animation frame:', thisFrame, nFrames);
      if(nFrames) {
        // self.svg.innerHTML = self.svgs[thisFrame];
        while(self.svg.firstChild)
          self.svg.removeChild(self.svg.firstChild);
        self.svg.appendChild(self.svgs[thisFrame]);
      } else {
        self.svg.innerText = "Loading...";
      }
      requestAnimationFrame(animate);
    };
    requestAnimationFrame(animate);
  }
  onLoad = ace => {
    setTimeout(function() {
      ace.resize();
    }, 0);
  }
  onChange = text => {
    this.setState({program: text});
    if (this.timeout)
      clearTimeout(this.timeout);
    const socket = this.state.socket
    const self = this;

    this.timeout = setTimeout(function() {
      console.log('change', text);
      self.setState(state => ({...state, message: "Compiling..."}));
      socket.send(text);
    }, 500);
  };
  selectPreset = evt => {
    this.onChange(evt.target.value);
  };
  render() {
    const {message, program} = this.state;
    return (
      <div className="App">
        <div>
          <select onChange={this.selectPreset}>
            { preset.map( (group, i) =>
              <optgroup key={i} label={group.name}>
                { group.programs.map( (elt, i) =>
                    <option value={elt.code} key={i}>{elt.name}</option>
                )}
              </optgroup>
            )}
          </select>
        </div>
        <div className="editor">
          <AceEditor
            mode="haskell"
            width="100%"
            height="100%"
            theme="monokai"
            fontSize={16}
            name="UNIQUE_ID_OF_DIV"
            value={program}
            onLoad={this.onLoad}
            onChange={this.onChange}
            focus={true}
            editorProps={{
              $blockScrolling: true
            }}/>
        </div>
        <div className="controls">

        </div>
        <div className="viewer">
          <div ref={node => this.svg = node}/>
          <div className="messages">
            <pre>{message}</pre>
          </div>
        </div>
      </div>
    );
  }
}

export default App;
