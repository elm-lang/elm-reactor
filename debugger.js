Elm.debugFullscreen = function(module, moduleFile, hotSwapState /* =undefined */) {
  segmentDisplay();
  var elmMain = document.getElementById("elmMain");
  return Elm.embed(Elm.debuggerAttach(module,hotSwapState), elmMain);
}

function segmentDisplay() {

  var elmMain = document.createElement("div");
  elmMain.id = "elmMain";
  elmMain.style.width = "100%";
  elmMain.style.height = "100%";

  var elmDebugger = document.createElement("div");
  elmDebugger.id = "elmDebugger";
  elmDebugger.style.background = "#eee";
  elmDebugger.style.width = "90%";
  elmDebugger.style.height = "60px";
  elmDebugger.style.position = "absolute";
  elmDebugger.style.top = 20 + "px";
  elmDebugger.style.left = window.innerWidth * 0.05 + "px";
  elmDebugger.style.opacity = "0.5";

  createDebuggerDisplay(elmDebugger);

  document.body.appendChild(elmMain);
  document.body.appendChild(elmDebugger);
}

function createDebuggerDisplay(debugDiv) {
  Elm.embed(Elm.DebuggerInterface, debugDiv);
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
