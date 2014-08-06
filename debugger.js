var elmDebugger = {
  restart: function() {},
  pause: function() {},
  kontinue: function() {},
  getMaxSteps: function() { return 0; },
  stepTo: function(i) {},
  getPaused: function() { return false; },
  getHotSwapState: function() { return null; },
  watchTracker: { frames:[{}] }
};

var mainHandle = {};
var debuggerHandle = {};
var createdSocket = false;
var filePath;

var ELM_MAIN_ID = "elmMain";

Elm.debugFullscreen = function(module, moduleFile, hotSwapState /* =undefined */) {
  filePath = moduleFile;
  segmentDisplay();
  var elmMain = document.getElementById(ELM_MAIN_ID);
  mainHandle = Elm.embed(Elm.debuggerAttach(module, hotSwapState), elmMain);
  return mainHandle;
}

function createMainElement() {
  var mainDiv = document.createElement("div");
  mainDiv.id = ELM_MAIN_ID;
  mainDiv.style.width = "100%";
  mainDiv.style.height = "100%";
  return mainDiv;
}

function createDebuggingElement() {
  var debuggerDiv = document.createElement("div");
  debuggerDiv.id = "elmDebugger";
  debuggerDiv.style.background = "#eee";
  debuggerDiv.style.width = "275px";
  debuggerDiv.style.height = "100%";
  debuggerDiv.style.position = "absolute";
  debuggerDiv.style.top = "0px";
  debuggerDiv.style.left = "0px";
  debuggerDiv.style.opacity = "0.5";
  return debuggerDiv;
}

function segmentDisplay() {
  var mainDiv = createMainElement();

  var debuggerDiv = createDebuggingElement();

  debuggerHandle = Elm.embed(Elm.DebuggerInterface, debuggerDiv, {eventCounter: 0, restartPort: false});
  debuggerHandle.ports.scrubTo.subscribe(scrubber);
  debuggerHandle.ports.pause.subscribe(elmPauser);
  debuggerHandle.ports.restart.subscribe(elmRestart);

  document.body.appendChild(mainDiv);
  document.body.appendChild(debuggerDiv);
}

parent.window.addEventListener("message", function(e) {
  if (e.data === "elmDebuggerInit") {
    elmDebugger = parent.Elm.Debugger;
    if (!createdSocket) {
      // initSocket();
    }
  } else if (e.data === "elmNotify") {
    debuggerHandle.ports.eventCounter.send(elmDebugger.getMaxSteps());
  }
}, false);

function scrubber(position) {
  if (elmDebugger.getPaused()) {
    elmDebugger.stepTo(position);
  }
}

function elmPauser(doPause) {
  if (doPause) {
    elmDebugger.pause();
  } else {
    elmDebugger.kontinue();
  }
}

function elmRestart() {
  debuggerHandle.ports.restartPort.send(true);
  debuggerHandle.ports.restartPort.send(false);
  elmDebugger.restart();
}


function initSocket() {
  createdSocket = true;
  // "/todo.html" => "todo.elm"
  filePath = filePath || window.location.pathname.substr(1).split(".")[0] + ".elm";
  var socketLocation = "ws://" + window.location.host + "/socket?file=" + filePath;
  var serverConnection = new WebSocket(socketLocation);
  serverConnection.onmessage = messageRoute;
}

function messageRoute(event) {
  console.log(event);
  hotSwap(event.data);
};

function hotSwap(raw) {
  var elmMain = document.getElementById(ELM_MAIN_ID);
  var result = JSON.parse(raw);
  var js = result.success;
  if (js) {
      var error = document.getElementById('ErrorMessage');
      if (error) {
          error.parentNode.removeChild(error);
      }
      window.eval(js);
      var moduleStr = js.match(/(Elm\..+)\ =\ \1/)[1];
      var module = window.eval(moduleStr);
      if (Elm.Debugger) {
          var debuggerState = Elm.Debugger.getHotSwapState();
          mainHandle.dispose();
          Elm.Debugger.dispose();

          var newMainNode = createMainElement();
          document.body.appendChild(newMainNode);

          var wrappedModule = Elm.debuggerAttach(module, debuggerState);
          mainHandle = Elm.embed(wrappedModule, newMainNode);
      }
      else {
          mainHandle = mainHandle.swap(module);
      }
  }
}
