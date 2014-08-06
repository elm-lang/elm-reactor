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

var debuggingPanelExpanded = false;

var ELM_MAIN_ID = "elmMain";
var ELM_DEBUGGER_ID = "elmDebugger";

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
  document.body.appendChild(mainDiv);
}

function createDebuggingElement() {
  var debuggerWidth = 275;
  var darkGrey = "#5C5C5C";

  var debugTools = document.createElement("div");
  debugTools.id = "debugTools"
  var debuggerDiv = document.createElement("div");
  debuggerDiv.id = ELM_DEBUGGER_ID;

  var debugToggleButton = document.createElement("div");
  debugToggleButton.id = "debugtoggle";
  debugToggleButton.style.position = "absolute";
  debugToggleButton.style.width = "30px";
  debugToggleButton.style.height = "50px";
  debugToggleButton.style.top = window.innerHeight / 2 + "px";
  debugToggleButton.style.left = "-30px";
  debugToggleButton.style.background = darkGrey;

  debugToggleButton.onclick = function() {
    var dd = document.getElementById("debugTools");
    if (debuggingPanelExpanded){
      dd.style.left = window.innerWidth + "px";
      debuggingPanelExpanded = false;
    } else {
      dd.style.left = window.innerWidth - debuggerWidth + "px";
      debuggingPanelExpanded = true;
    }
  }

  debugTools.style.background = darkGrey;
  debugTools.style.width = debuggerWidth + "px";
  debugTools.style.height = "100%";
  debugTools.style.position = "absolute";
  debugTools.style.top = "0px";
  debugTools.style.left = window.innerWidth - debuggerWidth + "px";
  debugTools.style.transitionDuration = "0.3s";
  
  debugTools.appendChild(debugToggleButton);
  debugTools.appendChild(debuggerDiv);
  document.body.appendChild(debugTools);
}

function segmentDisplay() {
  createMainElement();

  createDebuggingElement();
  var debuggerDiv = document.getElementById(ELM_DEBUGGER_ID);

  debuggerHandle = Elm.embed(Elm.DebuggerInterface, debuggerDiv, {eventCounter: 0});
  debuggerHandle.ports.scrubTo.subscribe(scrubber);
  debuggerHandle.ports.pause.subscribe(elmPauser);
  debuggerHandle.ports.restart.subscribe(elmRestart);
}

parent.window.addEventListener("message", function(e) {
  if (e.data === "elmDebuggerInit") {
    elmDebugger = parent.Elm.Debugger;
    if (!createdSocket) {
      initSocket();
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
  hotSwap(event.data);
};

function hotSwap(raw) {
  var debuggerDiv = document.getElementById(ELM_DEBUGGER_ID);
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
          debuggerDiv.parentElement.insertBefore(newMainNode,debuggerDiv);

          var wrappedModule = Elm.debuggerAttach(module, debuggerState);
          mainHandle = Elm.embed(wrappedModule, newMainNode);
      }
      else {
          mainHandle = mainHandle.swap(module);
      }
  }
}
