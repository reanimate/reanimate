const backend = "149.56.132.163";

function playgroundInit(elt) {
  var mySockets = {};
  var frames = {};

  function sendSocketCommand(wat) {
    if (wat.cmd == "connect") {
      socket = new WebSocket(wat.address);
      socket.onopen = function (event) {
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
    } else if (wat.cmd == "send") {
      mySockets[wat.name].send(wat.content);
    } else if (wat.cmd == "close") {
      mySockets[wat.name].close();
      delete mySockets[wat.name];
    }
  }

  var app = Elm.Main.init({
    node: document.getElementById(elt)
  });
  app.ports.sendSocketCommand.subscribe(sendSocketCommand);
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
      app.ports.receiveEditorMsg.send(code);
    }
  };
}
