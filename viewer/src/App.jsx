import React, {Component} from 'react';
import './App.css';

class App extends Component {
  connect = () => {
    // const ws = new WebSocket("wss://reanimate.clozecards.com:9160");
    const ws = new WebSocket("ws://localhost:9161");

    ws.onopen = event => {
      this.setState(state => ({
        ...state,
        message: "Connected."
      }));
      ws.send('60');
    }
    ws.onclose = event => {
      this.setState(state => ({
        ...state,
        message: "Disconnected."
      }));
      setTimeout(this.connect, 1000);
    }
    ws.onmessage = event => {
      if (event.data === "Success!") {
        console.log("Success");
      } else if (event.data === "Compiling") {
        this.setState({message: "Compiling..."});
        this.status = 'compiling';
        this.svgs = [];
        this.frame_count = 0;
        this.next_frame = 0;
      } else if (event.data === "Done") {
        this.setState({message: ""});
        console.log("Done");
        this.status = 'done';
      } else if (event.data.startsWith("Error")) {
        console.log("Error", event.data.substring(5));
        this.setState({message: event.data.substring(5)});
      } else {
        const num = parseInt(event.data);
        if(isNaN(num)) {
          // this.setState({message: `Rendering: ${this.nFrames_new}`});
          // this.nFrames_new++;
          // const div = document.createElement('div');
          // div.innerHTML = event.data;
          // this.svgs_new.push(div);
          const div = document.createElement('div');
          div.innerHTML = event.data;
          this.svgs[this.next_frame] = div;
          var count = 0;
          this.svgs.forEach(_ => count++);
          console.log('Received', this.next_frame, this.frame_count, count);
        } else {

          if( this.status === 'compiling' ) {
            this.setState({message: `Rendering...`});
            this.frame_count = num;
            this.status = 'rendering';
            this.start = Date.now();
            this.svgs = [];
            this.svgs[this.frame_count-1] = undefined;
          } else if( this.status === 'rendering' ) {
            this.next_frame = num;
          } else {
            console.log("Bad state change: received number");
          }
        }
      }
    }
    this.setState(state => ({
      ...state,
      socket: ws,
      message: "Connecting..."
    }));
  }
  constructor(props) {
    super(props);

    this.state = {
    };
    setTimeout(this.connect, 0);
    this.svgs = [];
    this.start = Date.now();

    this.status = '';
    this.frame_count = 0;
    this.next_frame = 0;

    const self = this;
    const animate = () => {
      const now = Date.now();
      const nFrames = self.frame_count;
      const aniDuration = self.frame_count/60;
      const thisFrame = (Math.round((now - this.start) / 1000 * 60)) % nFrames
      // const thisFrame = 0; console.log('Animation frame:', thisFrame, nFrames);
      var count = 0;
      this.svgs.forEach(_ => count++);
      if (nFrames) {
        // self.svg.innerHTML = self.svgs[thisFrame];
        if(self.svgs[thisFrame]) {
          // self.hud.innerText = '' + thisFrame + '/' + self.frame_count;
          this.setState({message: '' + thisFrame + '/' + self.frame_count + ' ' + Math.round(count/aniDuration) + ' fps'});
          while (self.svg.firstChild)
            self.svg.removeChild(self.svg.firstChild);
          self.svg.appendChild(self.svgs[thisFrame]);
        }
      } else {
        self.svg.innerText = "";
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
  render() {
    const {message} = this.state;
    return (
      <div className="App">
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
