const backend = "149.56.132.163";

var lastConnect;

function scheduleConnect(cb) {
  let minDelay = 2000;
  if( lastConnect === undefined ) {
    console.log('Connecting immediately');
    lastConnect = new Date().getTime();
    cb();
  } else {
    let now = new Date().getTime();
    let target = Math.max(now, lastConnect + minDelay);
    console.log('Connecting in', target-now);
    lastConnect = target;
    setTimeout(cb, target-now);
  }
}

function playgroundInit(elt) {
  var mySockets = {};
  var frames = {};
  var lastScript = "";

  function sendSocketCommand(wat) {
    if (wat.cmd == "connect") {
      scheduleConnect(function () {
        if ( mySockets[wat.name] ) return;
        socket = new WebSocket(wat.address);
        socket.onopen = function (event) {
          if( socket.readyState !== 1 ) {
            app.ports.receiveSocketMsg.send({
              name: wat.name,
              msg: "data",
              data: "connection failed"
            });
            return;
          }
          console.log('onopen', socket, mySockets);
          socket.send(lastScript);
          app.ports.receiveSocketMsg.send({
            name: wat.name,
            msg: "data",
            data: "connection established"
          });
        }
        socket.onmessage = function (event) {
          const lines = event.data.split('\n');
          const cmd = lines[0];
          if( cmd === "frame_count" ) {
            frames = {};
          } else if ( cmd === "frame") {
            // Prefetch image.
            const nth = parseInt(lines[1]);
            const url = lines[2];
            frames[nth] = new Image();
            frames[nth].src = "https://reanimate.clozecards.com/"+url;
          }
          app.ports.receiveSocketMsg.send({
            name: wat.name,
            msg: "data",
            data: event.data
          });
        }
        connectionFailedHandler = function (event) {
          app.ports.receiveSocketMsg.send({
            name: wat.name,
            msg: "data",
            data: "connection failed"
          });
        }
        socket.onerror = connectionFailedHandler;
        socket.onclose = connectionFailedHandler;
        mySockets[wat.name] = socket;
        console.log('mySockets set');
      });
    } else if (wat.cmd == "send") {
      if( mySockets[wat.name] && mySockets[wat.name].readyState === mySockets[wat.name].OPEN ) {
        mySockets[wat.name].send(wat.content);
      }
    } else if (wat.cmd == "close") {
      mySockets[wat.name].close();
      delete mySockets[wat.name];
    }
  }

  var app = Elm.Main.init({
    node: elt
  });
  app.ports.sendSocketCommand.subscribe(sendSocketCommand);
  var tHandler = setTimeout(function(){},0);
  return {
    play: function () {
      app.ports.receiveControlMsg.send('play');
    },
    pause: function () {
      app.ports.receiveControlMsg.send('pause');
    },
    seek1: function () {
      app.ports.receiveControlMsg.send('seek1');
    },
    seek10: function () {
      app.ports.receiveControlMsg.send('seek10');
    },
    seek_1: function () {
      app.ports.receiveControlMsg.send('seek-1');
    },
    seek_10: function () {
      app.ports.receiveControlMsg.send('seek-10');
    },
    newCode: function(code) {
      clearTimeout(tHandler);
      tHandler = setTimeout(function() {
        lastScript = code;
        app.ports.receiveEditorMsg.send(code);
      }, 500);
    }
  };
}

function renderSnippets(elt, myCodeMirror) {
  for(var i=0;i<snippets.length;i++) {
    const div = document.createElement("div");
    const title = document.createElement("span");
    const img = document.createElement("img");
    const code = snippets[i].code;
    title.innerHTML = snippets[i].title;
    img.src = snippets[i].url;
    div.appendChild(title)
    div.appendChild(img)
    div.onclick = function () {
      myCodeMirror.setValue(code);
      closeModal(document.querySelector('#examples-modal'));
    };
    elt.appendChild(div)
  }
}

function closeModal(modal) {
  modal.classList.remove('is-active');
}
function openModal(modal) {
  modal.classList.add('is-active');
}

// Add javascript handlers to modal for closing.
function configureModal(modal) {
  modal.querySelector('.modal-background').onclick = function () {
    modal.classList.remove('is-active');
  };
  modal.querySelector('.modal-close').onclick = function () {
    modal.classList.remove('is-active');
  };
  modal.querySelector('.delete').onclick = function () {
    modal.classList.remove('is-active');
  };
}
