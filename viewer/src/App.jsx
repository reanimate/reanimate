import { HotKeys, configure } from 'react-hotkeys';
import React, {Component} from 'react';
import './App.css';

configure({
  ignoreRepeatedEventsWhenKeyHeldDown: false
});

const keyMap = {
  PAUSE: "space",
  STEP_FORWARD: "right",
  STEP_BACKWARDS: "left",
  SKIP_FORWARD: "up",
  SKIP_BACKWARDS: "down"
}

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

      this.svgs = [];
      this.status = '';
      this.frame_count = 0;
      this.next_frame = 0;

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
          //console.log('Received', this.next_frame, this.frame_count, count);
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
  frameByTimer() {
    const now = Date.now();
    const nFrames = this.frame_count;
    return (Math.round((now - this.start) / 1000 * 60)) % nFrames;
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
    this.freeze_frame = null;

    const self = this;
    const animate = () => {
      const now = Date.now();
      const nFrames = self.frame_count;
      const aniDuration = self.frame_count/60;
      const thisFrame =
        (this.freeze_frame !== null)
          ? this.freeze_frame
          : self.frameByTimer();
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
    const self = this;
    const pause = () => {
      if(self.freeze_frame === null) {
        console.log('Pause')
        self.freeze_frame = self.frameByTimer();
      }
    }
    const keyHandlers = {
      PAUSE: event => {
          event.preventDefault();
          if(self.freeze_frame === null) {
            pause();
          } else {
            console.log('unpause')
            self.start = Date.now() - (self.freeze_frame/60*1000)
            self.freeze_frame = null;
          }
        },
      STEP_FORWARD: event => {
        pause();
        self.freeze_frame=(self.freeze_frame+1)%self.frame_count;
      },
      SKIP_FORWARD: event => {
        pause();
        self.freeze_frame=(self.freeze_frame+10)%self.frame_count;
      },
      STEP_BACKWARDS: event => {
        pause();
        self.freeze_frame-=1;
        if(self.freeze_frame<0) {
          self.freeze_frame = self.frame_count+self.freeze_frame;
        }
      },
      SKIP_BACKWARDS: event => {
        pause();
        self.freeze_frame-=10;
        if(self.freeze_frame<0) {
          self.freeze_frame = self.frame_count+self.freeze_frame;
        }
      }

    }
    const {message} = this.state;
    return (
      <HotKeys handlers={keyHandlers} keyMap={keyMap}>
        <div className="App">
          <div className="viewer">
            <div ref={node => this.svg = node}/>
            <div className="messages">
              <pre>{message}</pre>
            </div>
          </div>
        </div>
      </HotKeys>
    );
  }
}

export default App;
