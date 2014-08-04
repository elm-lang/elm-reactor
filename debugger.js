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

Elm.debugFullscreen = function(module, moduleFile, hotSwapState /* =undefined */) {
  segmentDisplay();
  var elmMain = document.getElementById("elmMain");
  mainHandle = Elm.embed(Elm.debuggerAttach(module,hotSwapState), elmMain);
  return mainHandle;
}

function segmentDisplay() {
  var mainDiv = document.createElement("div");
  mainDiv.id = "elmMain";
  mainDiv.style.width = "100%";
  mainDiv.style.height = "100%";

  var debuggerDiv = document.createElement("div");
  debuggerDiv.id = "elmDebugger";
  debuggerDiv.style.background = "#eee";
  debuggerDiv.style.width = "90%";
  debuggerDiv.style.height = "40px";
  debuggerDiv.style.position = "absolute";
  debuggerDiv.style.top = 20 + "px";
  debuggerDiv.style.left = window.innerWidth * 0.05 + "px";
  debuggerDiv.style.opacity = "0.5";

  debuggerHandle = Elm.embed(Elm.DebuggerInterface, debuggerDiv, {eventCounter: 0});
  debuggerHandle.ports.scrub.subscribe(scrubber);
  debuggerHandle.ports.pause.subscribe(elmPauser);
  debuggerHandle.ports.restart.subscribe(elmRestart);

  document.body.appendChild(mainDiv);
  document.body.appendChild(debuggerDiv);
}

parent.window.addEventListener("message", function(e) {
  if (e.data === "elmDebuggerInit") {
    elmDebugger = parent.Elm.Debugger;
  } else if (e.data === "elmNotify") {
    debuggerHandle.ports.eventCounter.send(elmDebugger.getMaxSteps());
  }
}, false);

function scrubber(position) {
  if (elmDebugger.getPaused()) {
    elmDebugger.stepTo(position);
    console.log(position);
  }
}

function elmPauser(paused) {
  console.log(paused);
  if (paused) {
    elmDebugger.pause();
  } else {
    elmDebugger.kontinue();
  }
}

function elmRestart() {
  console.log("restart");
  elmDebugger.restart();
}

// var createdSocket = false;
// parent.window.addEventListener("message", function(e) {
//   if (e.data == "elmDebuggerInit") {
//     if (!createdSocket) {
//       initSocket();
//     }
//     startDebugging(e.source);
//   }
//   else if (e.data == "elmNotify") {
//     refreshDebuggerUI();
//   }
// }, false);

// function initSocket() {
//   createdSocket = true;
//   var filePath = Elm.Debugger.filename;
//   // "/todo.html" => "todo.elm"
//   filePath = filePath || window.location.pathname.substr(1).split(".")[0] + ".elm";
//   var socketLocation = "ws://" + window.location.host + "/socket?file=" + filePath;
//   var serverConnection = new WebSocket(socketLocation);
//   serverConnection.onmessage = messageRoute;
// }

// function messageRoute(event) {
//   var hotSwapCheckbox = document.getElementById("hotswap");
//   if(hotSwapCheckbox.checked) {
//     hotSwap(event.data);
//   }
// };

// function hotSwap(raw) {
//   var top = self.parent;
//   var result = JSON.parse(raw);
//   var js = result.success;
//   if (js) {
//       var error = top.output.document.getElementById('ErrorMessage');
//       if (error) {
//           error.parentNode.removeChild(error);
//       }
//       top.output.eval(js);
//       var moduleStr = js.match(/(Elm\..+)\ =\ \1/)[1];
//       var module = top.output.eval(moduleStr);
//       if (top.output.Elm.Debugger) {
//           var debuggerState = top.output.Elm.Debugger.getHotSwapState();
//           if (top.output.runningElmModule) {
//             top.output.runningElmModule.dispose();
//           }
//           top.output.Elm.Debugger.dispose();

//           var wrappedModule = top.output.Elm.debuggerAttach(module, debuggerState);
//           top.output.runningElmModule = top.output.Elm.fullscreen(wrappedModule);
//       }
//       else {
//           top.output.runningElmModule =
//               top.output.runningElmModule.swap(module);
//       }
//   }
// }
