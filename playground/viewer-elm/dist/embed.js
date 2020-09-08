function embedPlayground(parent, extraClass) {
  const code = parent.innerText;
  const container = document.createElement("div");
  container.classList.add('playground-embed')
  if(typeof extraClass !== 'undefined') {
    container.classList.add(extraClass)
  }

  const editor = document.createElement("div");
  editor.classList.add('playground-editor')

  const viewContainer = document.createElement("div");
  viewContainer.classList.add('playground-viewer')

  const player = document.createElement("div");
  player.classList.add('playground-player')
  viewContainer.appendChild(player);

  const elm = document.createElement("div");
  player.appendChild(elm);
  
  var app = playgroundInit(elm);
  var playing = true;
  player.onclick = function () {
    if(playing)
      app.pause();
    else
      app.play();
    playing=!playing;
  };
  var myCodeMirror = CodeMirror(editor, {
    lineNumbers: false,
    value: code,
    mode: "haskell",
    lineWrapping: true
  });
  myCodeMirror.on('change', function () {
    app.pause();
    app.newCode(myCodeMirror.getValue());
  });
  app.newCode(myCodeMirror.getValue());
  setTimeout(function () {
    myCodeMirror.refresh();
  },0);

  container.appendChild(editor);
  container.appendChild(viewContainer);
  parent.replaceWith(container);
}
function embedPlaygroundAll(target = '.playground-code') {
  document.querySelectorAll(target).forEach(embedPlayground);
}
function embedPlaygroundCode(parent, code) {
  parent.innerText = code;
  embedPlayground(parent);
}
