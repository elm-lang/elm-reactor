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

var debuggingPanelExpanded = true;
var elmPauseState = false;
var elmPermitHotswaps = true;

var ELM_MAIN_ID = "elmMain";
var ELM_DEBUGGER_ID = "elmToolPanel";

function createMainElement() {
  var mainDiv = document.createElement("div");
  mainDiv.id = ELM_MAIN_ID;
  mainDiv.style.width = "100%";
  mainDiv.style.height = "100%";
  document.body.appendChild(mainDiv);
}

function createDebuggingElement() {
  var debuggerWidth = 275;
  var darkGrey = "#4A4A4A";

  var debugTools = document.createElement("div");
  debugTools.id = ELM_DEBUGGER_ID;

  var debuggerDiv = document.createElement("div");
  debuggerDiv.id = "elmDebugger";
  debuggerDiv.style.overflow = "hidden";

  // Create and style the panel
  debugTools.style.background = darkGrey;
  debugTools.style.width = debuggerWidth + "px";
  debugTools.style.height = "100%";
  debugTools.style.position = "absolute";
  debugTools.style.top = "0px";
  debugTools.style.left = window.innerWidth - debuggerWidth + "px";
  debugTools.style.transitionDuration = "0.3s";
  debugTools.style.opacity = 0.97;

  // Create and style the button
  var debugTab = document.createElement("div");
  debugTab.id = "debugtoggle";
  debugTab.style.position = "absolute";
  debugTab.style.width = "15px";
  debugTab.style.height = "30px";
  debugTab.style.top = window.innerHeight / 2 + "px";
  debugTab.style.left = "-15px";
  debugTab.style.borderTopLeftRadius = "3px";
  debugTab.style.borderBottomLeftRadius = "3px";
  debugTab.style.background = darkGrey;

  // Wire the button
  debugTab.onclick = function() {
    var toolPanel = document.getElementById("elmToolPanel");
    if (debuggingPanelExpanded){
      toolPanel.style.left = window.innerWidth + "px";
      toolPanel.style.width = "0px";
      debuggingPanelExpanded = false;
    } else {
      toolPanel.style.left = window.innerWidth - debuggerWidth + "px";
      toolPanel.style.width = debuggerWidth + "px";
      debuggingPanelExpanded = true;
    }
  }
  
  debugTools.appendChild(debugTab);
  debugTools.appendChild(debuggerDiv);
  document.body.appendChild(debugTools);
}




Elm.debugFullscreen = function(module, moduleFile, hotSwapState /* =undefined */) {
  filePath = moduleFile;
  segmentDisplay();
  var elmMain = document.getElementById(ELM_MAIN_ID);
  mainHandle = Elm.embed(Elm.debuggerAttach(module, hotSwapState), elmMain);
  return mainHandle;
}

function segmentDisplay() {
  createMainElement();

  createDebuggingElement();
  var debuggerDiv = document.getElementById("elmDebugger");

  debuggerHandle = Elm.embed(Elm.DebuggerInterface, debuggerDiv,
      { eventCounter: 0,
        watches: {}
      });
  debuggerHandle.ports.scrubTo.subscribe(scrubber);
  debuggerHandle.ports.pause.subscribe(elmPauser);
  debuggerHandle.ports.restart.subscribe(elmRestart);
  debuggerHandle.ports.permitHotswap.subscribe(elmHotswap);
}

parent.window.addEventListener("message", function(e) {
  if (e.data === "elmDebuggerInit") {
    elmDebugger = parent.Elm.Debugger;
    if (!createdSocket) {
      initSocket();
    }
  } else if (e.data === "elmNotify") {
    var currentPosition = elmDebugger.getMaxSteps();
    debuggerHandle.ports.eventCounter.send(currentPosition);
    sendWatches(currentPosition);
  }
}, false);

function scrubber(position) {
  if (elmDebugger.getPaused()) {
    elmDebugger.stepTo(position);
    sendWatches(position);
  }
}

function elmPauser(doPause) {
  if (doPause) {
    elmDebugger.pause();
  } else {
    elmDebugger.kontinue();
  }
  ELM_PAUSE_STATE = doPause;
}

function elmRestart() {
  elmDebugger.restart();
  sendWatches(0);
}

function elmHotswap(permitHotswaps) {
  elmPermitHotswaps = permitHotswaps;
}

function sendWatches(position) {
  var watchAtPoint = elmDebugger.watchTracker.frames[position];
  var watchList = showWatches(watchAtPoint);
  debuggerHandle.ports.watches.send(watchAtPoint);
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
  if (elmPermitHotswaps) {
    hotSwap(event.data);
  }
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

          createMainElement();
          var newMainNode = document.getElementById(ELM_MAIN_ID);
          debuggerDiv.parentElement.insertBefore(newMainNode,debuggerDiv);

          var wrappedModule = Elm.debuggerAttach(module, debuggerState);
          mainHandle = Elm.embed(wrappedModule, newMainNode);
      }
      else {
          mainHandle = mainHandle.swap(module);
      }
  }
}

function showWatches(frame) {
  var output = [];
  for (key in frame) {
    var value = frame[key];
  }
  return [["",""]];
}
