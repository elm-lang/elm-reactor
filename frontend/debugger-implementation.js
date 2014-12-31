(function() {
'use strict';

if (typeof window != 'undefined' && !window.location.origin) {
  window.location.origin =
      window.location.protocol + "//" +
      window.location.hostname +
      (window.location.port ? (':' + window.location.port) : '');
}


Elm.fullscreenDebug =
  fullscreenDebugWithOptions({ externalSwap: false });

Elm.fullscreenDebugWithOptions =
  fullscreenDebugWithOptions;


function fullscreenDebugWithOptions(options) {

    return function(elmModule, elmModuleFile, swapState /* =undefined */) {
        var createdSocket = false;
        var elmPermitSwaps = true;

        var ELM_DEBUGGER_ID = "elmToolPanel";
        var ELM_DARK_GREY = "#4A4A4A";
        var ELM_LIGHT_GREY = "#E4E4E4";

        var mainHandle = fullscreenDebugHooks(elmModule, swapState);
        var debuggerHandle = initDebugger();
        if (!options.externalSwap) {
            initSocket();
        }

        parent.window.addEventListener("message", function(e) {
            if (e.data === "elmNotify") {
                var currentPosition = mainHandle.debugger.getMaxSteps();
                if (debuggerHandle.ports) {
                    debuggerHandle.ports.eventCounter.send(currentPosition);
                    sendWatches(currentPosition);
                }
            }
        }, false);

        function createDebuggingElement() {
            var debuggingPanelExpanded = true;
            var debuggerWidth = 275;

            var debugTools = document.createElement("div");
            debugTools.id = ELM_DEBUGGER_ID;

            var debuggerDiv = document.createElement("div");
            debuggerDiv.id = "elmDebugger";
            debuggerDiv.style.overflow = "hidden";
            debuggerDiv.style.height = "100%";

            // Create and style the panel
            debugTools.style.background = ELM_DARK_GREY;
            debugTools.style.width = debuggerWidth + "px";
            debugTools.style.height = "100%";
            debugTools.style.position = "absolute";
            debugTools.style.top = "0px";
            debugTools.style.right = "0px";
            debugTools.style.transitionDuration = "0.3s";
            debugTools.style.opacity = 0.97;
            debugTools.style.zIndex = 1;

            // Prevent clicks from reaching the main elm instance under the panel
            function stopEvents(e) {
                if (!e) {
                    var e = window.event;
                }
                e.cancelBubble = true;
                if (e.stopPropagation) {
                    e.stopPropagation();
                }
            }
            debugTools.addEventListener("click", stopEvents);

            // Create and style the button
            var tabWidth = 25;
            var debugTab = document.createElement("div");
            debugTab.id = "debugToggle";
            debugTab.style.position = "absolute";
            debugTab.style.width = tabWidth + "px";
            debugTab.style.height = "60px";
            debugTab.style.top = window.innerHeight / 2 + "px";
            debugTab.style.left = "-" + tabWidth + "px";
            debugTab.style.borderTopLeftRadius = "3px";
            debugTab.style.borderBottomLeftRadius = "3px";
            debugTab.style.background = ELM_DARK_GREY;


            // Wire the button
            debugTab.onclick = function() {
                var toolPanel = document.getElementById("elmToolPanel");
                if (debuggingPanelExpanded){
                    toolPanel.style.width = "0px";
                    debuggingPanelExpanded = false;
                } else {
                    toolPanel.style.right = "0px";
                    toolPanel.style.width = debuggerWidth + "px";
                    debuggingPanelExpanded = true;
                }
            };

            debugTools.appendChild(debugTab);
            debugTools.appendChild(debuggerDiv);
            return debugTools;
        }

        function initDebugger() {
            function scrubber(position) {
                if (mainHandle.debugger.getPaused()) {
                    mainHandle.debugger.stepTo(position);
                    sendWatches(position);
                }
            }

            function elmPauser(doPause) {
                if (doPause) {
                  mainHandle.debugger.pause();
                } else {
                    mainHandle.debugger.kontinue();
                }
            }

            function elmRestart() {
                mainHandle.debugger.restart();
                sendWatches(0);
            }

            function elmSwap(permitSwaps) {
                elmPermitSwaps = permitSwaps;
            }

            var debugTools = createDebuggingElement();
            document.body.appendChild(debugTools);
            var debuggerDiv = document.getElementById("elmDebugger");

            var handle = Elm.embed(Elm.SideBar, debuggerDiv, {
                eventCounter: 0,
                watches: [],
                showSwap: !options.externalSwap
            });
            handle.ports.scrubTo.subscribe(scrubber);
            handle.ports.pause.subscribe(elmPauser);
            handle.ports.restart.subscribe(elmRestart);
            handle.ports.permitSwap.subscribe(elmSwap);
            return handle;
        }

        function sendWatches(position) {
            var separator = "  ";
            var output = [];

            var watchAtPoint = mainHandle.debugger.watchTracker.frames[position];

            for (var key in watchAtPoint) {
                var value = watchAtPoint[key];
                // The toString object is defined in toString.js
                // and is prepended to this file at build time.
                var stringified = prettyPrint(value, separator);
                output.push([key, stringified]);
            }
            debuggerHandle.ports.watches.send(output);
        }

        function initSocket() {
            createdSocket = true;
            // "/todo.html" => "todo.elm"
            elmModuleFile = elmModuleFile || window.location.pathname.substr(1).split(".")[0] + ".elm";
            var socketLocation = "ws://" + window.location.host + "/socket?file=" + elmModuleFile;
            var serverConnection = new WebSocket(socketLocation);
            serverConnection.onmessage = function(event) {
                if (elmPermitSwaps && debuggerHandle.ports) {
                    swap(event.data);
                }
            };
            window.addEventListener("unload", function() {
                serverConnection.close();
            });
        }

        function swap(raw) {
            var error = document.getElementById('ErrorMessage');
            if (error) {
                error.parentNode.removeChild(error);
            }

            var result = JSON.parse(raw);
            var code = result.code;
            var errorMessage = result.error;

            if (code) {
                window.eval(code);
                var elmModule = window.eval('Elm.' + result.name);
                if (mainHandle.debugger) {
                    var debuggerState = mainHandle.debugger.getSwapState();
                    mainHandle.debugger.dispose();
                    mainHandle.dispose();

                    mainHandle = Elm.fullscreenDebugHooks(elmModule, debuggerState);

                    // The div that rejects events must be after Elm
                    var ignoringDiv = document.getElementById("elmEventIgnorer");
                    if (ignoringDiv) {
                        ignoringDiv.parentNode.appendChild(ignoringDiv);
                    }
                }
                else {
                    mainHandle = mainHandle.swap(elmModule);
                }
            } else if (errorMessage) {
                var errorNode = document.createElement("pre");
                errorNode.id = "ErrorMessage";
                errorNode.innerHTML = errorMessage;
                errorNode.style.zindex = 1;
                errorNode.style.position = "absolute";
                errorNode.style.top = "0";
                errorNode.style.left = "0";
                errorNode.style.color = ELM_DARK_GREY;
                errorNode.style.backgroundColor = ELM_LIGHT_GREY;
                errorNode.style.padding = "1em";
                errorNode.style.margin = "1em";
                errorNode.style.borderRadius = "10px";

                document.body.appendChild(errorNode);
            }
        }

        if (options.externalSwap) {
            mainHandle.debugger.swap = swap;
        }
        return mainHandle;
    };
};


function fullscreenDebugHooks(elmModule, debuggerHistory /* =undefined */) {
  var exposedDebugger = {};
  function debuggerAttach(elmModule, debuggerHistory) {
    return {
      make: function(runtime) {
        var wrappedModule = debugModule(elmModule, runtime);
        exposedDebugger = debuggerInit(wrappedModule, runtime, debuggerHistory);
        return wrappedModule.debuggedModule;
      }
    }
  }
  var mainHandle = Elm.fullscreen(debuggerAttach(elmModule, debuggerHistory));
  mainHandle.debugger = exposedDebugger;
  return mainHandle;
}


var EVENTS_PER_SAVE = 100;


function debugModule(elmModule, runtime) {
  var programPaused = false;
  var recordedEvents = [];
  var asyncCallbacks = [];
  var snapshots = [];
  var watchTracker = Elm.Native.Debug.make(runtime).watchTracker;
  var pauseTime = 0;
  var eventsUntilSnapshot = EVENTS_PER_SAVE;
  runtime.debuggerStatus = runtime.debuggerStatus || {};
  runtime.debuggerStatus.eventCounter = runtime.debuggerStatus.eventCounter || 0;

  // runtime is the prototype of wrappedRuntime
  // so we can access all runtime properties too
  var wrappedRuntime = Object.create(runtime);
  wrappedRuntime.notify = notifyWrapper;
  wrappedRuntime.setTimeout = setTimeoutWrapper;

  // make a copy of the wrappedRuntime
  var assignedPropTracker = Object.create(wrappedRuntime);
  var debuggedModule = elmModule.make(assignedPropTracker);

  // make sure the signal graph is actually a signal & extract the visual model
  if ( !('recv' in debuggedModule.main) ) {
    debuggedModule.main = Elm.Signal.make(runtime).constant(debuggedModule.main);
  }

  // The main module stores imported modules onto the runtime.
  // To ensure only one instance of each module is created,
  // we assign them back on the original runtime object.
  Object.keys(assignedPropTracker).forEach(function(key) {
    runtime[key] = assignedPropTracker[key];
  });

  var signalGraphNodes = flattenSignalGraph(wrappedRuntime.inputs);
  var tracePath = tracePathInit(runtime, debuggedModule.main);

  snapshots.push(snapshotSignalGraph(signalGraphNodes));

  function notifyWrapper(id, v) {
    var timestep = runtime.timer.now();

    if (programPaused) {
      // ignore async events generated while playing back
      // or user events while program is paused
      return false;
    }
    else {
      recordEvent(id, v, timestep);
      var changed = runtime.notify(id, v, timestep);
      snapshotOnCheckpoint();
      if (parent.window) {
        parent.window.postMessage("elmNotify", window.location.origin);
      }
      return changed;
    }
  };

  function setTimeoutWrapper(func, delayMs) {
    if (programPaused) {
      // Don't push timers and such to the callback stack while we're paused.
      // It causes too many callbacks to be fired during unpausing.
      return 0;
    }
    var cbObj = { func:func, delayMs:delayMs, timerId:0, executed:false };
    var timerId = setTimeout(function() {
        cbObj.executed = true;
        func();
      }, delayMs);
    cbObj.timerId = timerId;
    asyncCallbacks.push(cbObj);
    return timerId;
  }

  function recordEvent(id, v, timestep) {
    watchTracker.pushFrame();
    recordedEvents.push({ id:id, value:v, timestep:timestep });
    runtime.debuggerStatus.eventCounter += 1;
  }

  function clearAsyncCallbacks() {
    asyncCallbacks.forEach(function(timer) {
      if (!timer.executed) {
        clearTimeout(timer.timerId);
      }
    });
  }

  function clearRecordedEvents() {
    recordedEvents = [];
    runtime.debuggerStatus.eventCounter = 0;
  }

  function getRecordedEventsLength() {
    return recordedEvents.length;
  }

  function getRecordedEventAt(i) {
    return recordedEvents[i];
  }

  function copyRecordedEvents() {
    return recordedEvents.slice();
  }

  function loadRecordedEvents(events) {
    recordedEvents = events.slice();
  }

  function clearSnapshots() {
    snapshots = [snapshotSignalGraph(signalGraphNodes)];
  }

  function getSnapshotAt(i) {
    var snapshotEvent = Math.floor(i / EVENTS_PER_SAVE);
    assert(snapshotEvent < snapshots.length && snapshotEvent >= 0,
           "Out of bounds index: " + snapshotEvent);
    return snapshots[snapshotEvent];
  }

  function snapshotOnCheckpoint() {
    if (eventsUntilSnapshot === 1) {
      snapshots.push(snapshotSignalGraph(signalGraphNodes));
      eventsUntilSnapshot = EVENTS_PER_SAVE;
    } else {
      eventsUntilSnapshot -= 1;
    }
  }

  function setPaused() {
    programPaused = true;
    clearAsyncCallbacks();
    pauseTime = Date.now();
    tracePath.stopRecording();
    preventInputEvents();
  }

  function setContinue(position) {
    var pauseDelay = Date.now() - pauseTime;
    runtime.timer.addDelay(pauseDelay);
    programPaused = false;

    // we need to dump the events that are ahead of where we're continuing.
    var lastSnapshotPosition = Math.floor(position / EVENTS_PER_SAVE);
    eventsUntilSnapshot = EVENTS_PER_SAVE - (position % EVENTS_PER_SAVE);
    snapshots = snapshots.slice(0, lastSnapshotPosition + 1);

    if (position < getRecordedEventsLength()) {
      var lastEventTime = recordedEvents[position].timestep;
      var scrubTime = runtime.timer.now() - lastEventTime;
      runtime.timer.addDelay(scrubTime);
    }

    recordedEvents = recordedEvents.slice(0, position);
    tracePath.clearTracesAfter(position);
    runtime.debuggerStatus.eventCounter = position;
    executeCallbacks(asyncCallbacks);
    permitInputEvents();

    tracePath.startRecording();
  }

  function getPaused() {
    return programPaused;
  }

  function preventInputEvents(){
    var events =
      [ "click", "mousemove", "mouseup", "mousedown", "mouseclick"
      , "keydown", "keypress", "keyup", "touchstart", "touchend"
      , "touchcancel", "touchleave", "touchmove", "pointermove"
      , "pointerdown", "pointerup", "pointerover", "pointerout"
      , "pointerenter", "pointerleave", "pointercancel"
      ];

    var ignore = function(e) {
      var evt = e ? e : window.event;
      if (evt.stopPropagation) {
        evt.stopPropagation();
      }
      if (evt.cancelBubble !== null) {
        evt.cancelBubble = true;
      }
      if (evt.preventDefault) {
        evt.preventDefault();
      }
      return false;
    };

    var ignoringDiv = document.getElementById("elmEventIgnorer");
    if (!ignoringDiv) {
      ignoringDiv = document.createElement("div");
      ignoringDiv.id = "elmEventIgnorer";
      ignoringDiv.style.position = "absolute";
      ignoringDiv.style.top = "0px";
      ignoringDiv.style.left = "0px";
      ignoringDiv.style.width = "100%";
      ignoringDiv.style.height = "100%";

      for (var i = events.length; i-- ;) {
        ignoringDiv.addEventListener(events[i], ignore, true);
      }
      runtime.node.appendChild(ignoringDiv);
    }
  }

  function permitInputEvents(){
    var ignoringDiv = document.getElementById("elmEventIgnorer");
    ignoringDiv.parentNode.removeChild(ignoringDiv);
  }

  return {
    debuggedModule: debuggedModule,
    signalGraphNodes: signalGraphNodes,
    initialSnapshot: snapshotSignalGraph(signalGraphNodes),
    initialAsyncCallbacks: asyncCallbacks.slice(),
    // API functions
    clearAsyncCallbacks: clearAsyncCallbacks,
    clearRecordedEvents: clearRecordedEvents,
    getRecordedEventsLength: getRecordedEventsLength,
    getRecordedEventAt: getRecordedEventAt,
    copyRecordedEvents: copyRecordedEvents,
    loadRecordedEvents: loadRecordedEvents,
    clearSnapshots: clearSnapshots,
    getSnapshotAt: getSnapshotAt,
    snapshotOnCheckpoint: snapshotOnCheckpoint,
    getPaused: getPaused,
    setPaused: setPaused,
    setContinue: setContinue,
    tracePath: tracePath,
    watchTracker: watchTracker
  };
}

// The debuggerHistory variable is passed in on swap. It represents
// the a state of the debugger for it to assume during init. It contains
// the paused state of the debugger, the recorded events, and the current
// event being processed.
function debuggerInit(elmModule, runtime, debuggerHistory /* =undefined */) {
  var currentEventIndex = 0;

  function resetProgram(position) {
    var closestSnapshot = elmModule.getSnapshotAt(position);
    elmModule.clearAsyncCallbacks();
    restoreSnapshot(elmModule.signalGraphNodes, closestSnapshot);
    redrawGraphics();
  }

  function restartProgram() {
    pauseProgram();
    resetProgram(0);
    elmModule.watchTracker.clear();
    elmModule.tracePath.clearTraces();
    elmModule.setContinue(0);
    elmModule.clearRecordedEvents();
    elmModule.clearSnapshots();
    executeCallbacks(elmModule.initialAsyncCallbacks);
  }

  function pauseProgram() {
    elmModule.setPaused();
    currentEventIndex = elmModule.getRecordedEventsLength();
  }

  function continueProgram() {
    if (elmModule.getPaused())
    {
      var closestSnapshotIndex =
          Math.floor(currentEventIndex / EVENTS_PER_SAVE) * EVENTS_PER_SAVE;
      resetProgram(currentEventIndex);
      var continueIndex = currentEventIndex;
      currentEventIndex = closestSnapshotIndex;
      stepTo(continueIndex);
      elmModule.setContinue(currentEventIndex);
    }
  }

  function stepTo(index) {
    if (!elmModule.getPaused()) {
      elmModule.setPaused();
      resetProgram();
    }

    if (index < 0 || index > getMaxSteps()) {
      throw "Index out of bounds: " + index;
    }

    if (index < currentEventIndex) {
      var closestSnapshotIndex = Math.floor(index / EVENTS_PER_SAVE) * EVENTS_PER_SAVE;
      resetProgram(index);
      currentEventIndex = closestSnapshotIndex;
    }

    while (currentEventIndex < index) {
      var nextEvent = elmModule.getRecordedEventAt(currentEventIndex);
      runtime.notify(nextEvent.id, nextEvent.value, nextEvent.timestep);

      currentEventIndex += 1;
    }
  }

  function getMaxSteps() {
    return elmModule.getRecordedEventsLength();
  }

  function redrawGraphics() {
    var main = elmModule.debuggedModule.main
    for (var i = main.kids.length ; i-- ; ) {
      main.kids[i].recv(runtime.timer.now(), true, main.id);
    }
  }

  function getSwapState() {
    var continueIndex = currentEventIndex;
    if (!elmModule.getPaused()) {
      continueIndex = getMaxSteps();
    }
    return {
      paused: elmModule.getPaused(),
      recordedEvents: elmModule.copyRecordedEvents(),
      currentEventIndex: continueIndex
    };
  }

  function dispose() {
    var parentNode = runtime.node.parentNode;
    parentNode.removeChild(elmModule.tracePath.canvas);
    parentNode.removeChild(runtime.node);
  }

  if (debuggerHistory) {
    // The problem is that we want to previous paused state. But
    // by the time JS reaches here, the old code has been swapped out
    // and the new modules are being generated. So we can ask the
    // debugging console what it thinks the pause state is and go
    // from there.
    var paused = debuggerHistory.paused;
    elmModule.setPaused();
    elmModule.loadRecordedEvents(debuggerHistory.recordedEvents);
    var index = getMaxSteps();
    runtime.debuggerStatus.eventCounter = 0;
    elmModule.tracePath.clearTraces();

    // draw new trace path
    elmModule.tracePath.startRecording();
    while(currentEventIndex < index) {
      var nextEvent = elmModule.getRecordedEventAt(currentEventIndex);
      runtime.debuggerStatus.eventCounter += 1;
      runtime.notify(nextEvent.id, nextEvent.value, nextEvent.timestep);
      elmModule.snapshotOnCheckpoint();
      currentEventIndex += 1;
    }
    elmModule.tracePath.stopRecording();

    stepTo(debuggerHistory.currentEventIndex);
    if (!paused) {
      elmModule.setContinue(debuggerHistory.currentEventIndex);
    }
  }

  runtime.node.parentNode.appendChild(elmModule.tracePath.canvas);

  var elmDebugger = {
      restart: restartProgram,
      pause: pauseProgram,
      kontinue: continueProgram,
      getMaxSteps: getMaxSteps,
      stepTo: stepTo,
      getPaused: elmModule.getPaused,
      getSwapState: getSwapState,
      dispose: dispose,
      allNodes: elmModule.signalGraphNodes,
      watchTracker: elmModule.watchTracker
  };

  return elmDebugger;
}

function Point(x, y) {
  this.x = x;
  this.y = y;

  this.translate = function(x, y) {
    return new Point(this.x + x, this.y + y);
  }

  this.equals = function(p) {
    return this.x == p.x && this.y == p.y;
  }
}

function tracePathInit(runtime, signalGraphMain) {
  var List = Elm.List.make(runtime);
  var Signal = Elm.Signal.make(runtime);
  var tracePathNode = A2(Signal.map, graphicsUpdate, signalGraphMain);
  var tracePathCanvas = createCanvas();
  var tracePositions = {};
  var recordingTraces = true;

  function findPositions(currentScene) {
    var positions = {};
    function processElement(elem, offset) {
      if (elem.element.ctor == "Custom" && elem.element.type == "Collage")
      {
        List.map(F2(processForm)(offset))(elem.element.model.forms);
      }
    }

    function processForm(offset, form) {
      if (form.form.ctor == "FElement")
      {
        processElement(form.form._0, offset.translate(form.x, -form.y));
      }
      if (form.form.ctor == "FGroup")
      {
        var newOffset = offset.translate(form.x, -form.y);
        List.map(F2(processForm)(newOffset))(form.form._1);
      }
      if (form.debugTracePathId)
      {
        positions[form.debugTracePathId] = new Point(form.x + offset.x, -form.y + offset.y);
      }
    }

    processElement(currentScene, new Point(0, 0));
    return positions;
  }

  function appendPositions(positions) {
    for (var id in positions) {
      var pos = positions[id];
      if (tracePositions.hasOwnProperty(id)) {
        tracePositions[id].push(pos);
      }
      else {
        tracePositions[id] = [pos];
      }
      if (tracePositions[id].length < runtime.debuggerStatus.eventCounter) {
        var padCount = runtime.debuggerStatus.eventCounter - tracePositions[id].length;
        var lastTracePosition = tracePositions[id][tracePositions[id].length - 1];
        for (var i = padCount; i--;) {
          tracePositions[id].push(lastTracePosition)
        }
      }
      assert(tracePositions[id].length === runtime.debuggerStatus.eventCounter,
             "We don't have a 1-1 mapping of trace positions to events");
    }
  }

  function graphicsUpdate(currentScene) {
    if (!recordingTraces) {
      return;
    }

    var ctx = tracePathCanvas.getContext('2d');
    ctx.clearRect(0, 0, tracePathCanvas.width, tracePathCanvas.height);

    ctx.save();
    ctx.translate(ctx.canvas.width/2, ctx.canvas.height/2);
    appendPositions(findPositions(currentScene));
    for (var id in tracePositions)
    {
      ctx.beginPath();
      var points = tracePositions[id];
      for (var i=0; i < points.length; i++)
      {
        var p = points[i];
        if (i == 0) {
          ctx.moveTo(p.x, p.y);
        }
        else {
          ctx.lineTo(p.x, p.y);
        }
      }
      ctx.lineWidth = 1;
      ctx.strokeStyle = "rgba(50, 50, 50, 0.4)";
      ctx.stroke();
    }

    ctx.restore();
  }

  function clearTraces() {
    tracePositions = {};
  }

  function stopRecording() {
    recordingTraces = false;
  }

  function startRecording() {
    recordingTraces = true;
  }

  function clearTracesAfter(position) {
    var newTraces = {};
    for (var id in tracePositions) {
      newTraces[id] = tracePositions[id].slice(0,position);
    }
    tracePositions = newTraces;
  }

  return {
    graphicsUpdate: graphicsUpdate,
    canvas: tracePathCanvas,
    clearTraces: clearTraces,
    clearTracesAfter: clearTracesAfter,
    stopRecording: stopRecording,
    startRecording: startRecording
  }
}

function executeCallbacks(callbacks) {
  callbacks.forEach(function(timer) {
    if (!timer.executed) {
      var func = timer.func;
      timer.executed = true;
      func();
    }
  });
}

function createCanvas() {
  var c = document.createElement('canvas');
  c.width = window.innerWidth;
  c.height = window.innerHeight;
  c.style.position = "absolute";
  c.style.top = "0";
  c.style.left = "0";
  c.style.pointerEvents = "none";
  return c;
}

function assert(bool, msg) {
  if (!bool) {
    throw "Assertion error: " + msg;
  }
}

function snapshotSignalGraph(signalGraphNodes) {
  var nodeValues = [];

  signalGraphNodes.forEach(function(node) {
    nodeValues.push({ value: node.value, id: node.id });
  });

  return nodeValues;
};

function restoreSnapshot(signalGraphNodes, snapshot) {
  assert(signalGraphNodes.length == snapshot.length,
         "saved program state has wrong length");
  for (var i=0; i < signalGraphNodes.length; i++) {
    var node = signalGraphNodes[i];
    var state = snapshot[i];
    assert(node.id == state.id, "the nodes moved position");

    node.value = state.value;
  }
}

function flattenSignalGraph(nodes) {
  var nodesById = {};

  function addAllToDict(node) {
    nodesById[node.id] = node;
    node.kids.forEach(addAllToDict);
  }
  nodes.forEach(addAllToDict);

  var allNodes = Object.keys(nodesById).sort().map(function(key) {
    return nodesById[key];
  });
  return allNodes;
}

var prettyPrint = function(){

    var independentRuntime = {};
    var List;
    var ElmArray;
    var Dict;

    var toString = function(v, separator) {
        var type = typeof v;
        if (type === "function") {
            var name = v.func ? v.func.name : v.name;
            return '<function' + (name === '' ? '' : ': ') + name + '>';
        } else if (type === "boolean") {
            return v ? "True" : "False";
        } else if (type === "number") {
            return v.toFixed(2).replace(/\.0+$/g, '');
        } else if ((v instanceof String) && v.isChar) {
            return "'" + addSlashes(v) + "'";
        } else if (type === "string") {
            return '"' + addSlashes(v) + '"';
        } else if (type === "object" && '_' in v && probablyPublic(v)) {
            var output = [];
            for (var k in v._) {
                for (var i = v._[k].length; i--; ) {
                    output.push(k + " = " + toString(v._[k][i], separator));
                }
            }
            for (var k in v) {
                if (k === '_') continue;
                output.push(k + " = " + toString(v[k], separator));
            }
            if (output.length === 0) return "{}";
            var body = "\n" + output.join(",\n");
            return "{" + body.replace(/\n/g,"\n" + separator) + "\n}";
        } else if (type === "object" && 'ctor' in v) {
            if (v.ctor.substring(0,6) === "_Tuple") {
                var output = [];
                for (var k in v) {
                    if (k === 'ctor') continue;
                    output.push(toString(v[k], separator));
                }
                return "(" + output.join(", ") + ")";
            } else if (v.ctor === "_Array") {
                if (!ElmArray) {
                    ElmArray = Elm.Array.make(independentRuntime);
                }
                var list = ElmArray.toList(v);
                return "Array.fromList " + toString(list, separator);
            } else if (v.ctor === "::") {
                var output = '[\n' + toString(v._0, separator);
                v = v._1;
                while (v && v.ctor === "::") {
                    output += ",\n" + toString(v._0, separator);
                    v = v._1;
                }
                return output.replace(/\n/g,"\n" + separator) + "\n]";
            } else if (v.ctor === "[]") {
                return "[]";
            } else if (v.ctor === "RBNode" || v.ctor === "RBEmpty") {
                if (!Dict || !List) {
                    Dict = Elm.Dict.make(independentRuntime);
                    List = Elm.List.make(independentRuntime);
                }
                var list = Dict.toList(v);
                var name = "Dict";
                if (list.ctor === "::" && list._0._1.ctor === "_Tuple0") {
                    name = "Set";
                    list = A2(List.map, function(x){return x._0}, list);
                }
                return name + ".fromList " + toString(list, separator);
            } else {
                var output = "";
                for (var i in v) {
                    if (i === 'ctor') continue;
                    var str = toString(v[i], separator);
                    var parenless = str[0] === '{' ||
                                    str[0] === '<' ||
                                    str[0] === "[" ||
                                    str.indexOf(' ') < 0;
                    output += ' ' + (parenless ? str : "(" + str + ')');
                }
                return v.ctor + output;
            }
        }
        if (type === 'object' && 'recv' in v) return '<signal>';
        return "<internal structure>";
    };

    function addSlashes(str) {
        return str.replace(/\\/g, '\\\\')
                  .replace(/\n/g, '\\n')
                  .replace(/\t/g, '\\t')
                  .replace(/\r/g, '\\r')
                  .replace(/\v/g, '\\v')
                  .replace(/\0/g, '\\0')
                  .replace(/\'/g, "\\'")
                  .replace(/\"/g, '\\"');
    }

    function probablyPublic(v) {
        var keys = Object.keys(v);
        var len = keys.length;
        if (len === 3
            && 'props' in v
            && 'element' in v) return false;
        if (len === 5
            && 'horizontal' in v
            && 'vertical' in v
            && 'x' in v
            && 'y' in v) return false;
        if (len === 7
            && 'theta' in v
            && 'scale' in v
            && 'x' in v
            && 'y' in v
            && 'alpha' in v
            && 'form' in v) return false;
        return true;
    }

    return toString;
}();


}());
