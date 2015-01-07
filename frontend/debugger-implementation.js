(function() {
'use strict';

function assert(bool, msg) {
    if (!bool) {
        throw new Error("Assertion error: " + msg);
    }
}


// SIDE BAR

var SIDE_BAR_ID = "elm-reactor-side-bar";
var SIDE_BAR_BODY_ID = "elm-reactor-side-bar-body";
var SIDE_BAR_WIDTH = 275;

var DARK_GREY = "#4A4A4A";
var LIGHT_GREY = "#E4E4E4";

function createSideBar() {
    var debuggingPanelExpanded = true;

    var sideBar = document.createElement("div");
    sideBar.id = SIDE_BAR_ID;

    var sideBarBody = document.createElement("div");
    sideBarBody.id = SIDE_BAR_BODY_ID;
    sideBarBody.style.overflow = "hidden";
    sideBarBody.style.height = "100%";

    // Create and style the panel
    sideBar.style.background = DARK_GREY;
    sideBar.style.width = SIDE_BAR_WIDTH + "px";
    sideBar.style.height = "100%";
    sideBar.style.position = "absolute";
    sideBar.style.top = "0px";
    sideBar.style.right = "0px";
    sideBar.style.transitionDuration = "0.3s";
    sideBar.style.opacity = 0.97;
    sideBar.style.zIndex = 1;

    // Prevent clicks from reaching the main elm instance under the panel
    sideBar.addEventListener("click", blockClicks);
    function blockClicks(e) {
        var event = e || window.event;
        event.cancelBubble = true;
        if (event.stopPropagation) {
            event.stopPropagation();
        }
    }

    // Create and style the button
    var tabWidth = 25;
    var sideBarTab = document.createElement("div");
    sideBarTab.id = "debugToggle";
    sideBarTab.style.position = "absolute";
    sideBarTab.style.width = tabWidth + "px";
    sideBarTab.style.height = "60px";
    sideBarTab.style.top = "50%";
    sideBarTab.style.left = "-" + tabWidth + "px";
    sideBarTab.style.borderTopLeftRadius = "3px";
    sideBarTab.style.borderBottomLeftRadius = "3px";
    sideBarTab.style.background = DARK_GREY;


    // Wire the button
    sideBarTab.onclick = function() {
        var toolPanel = document.getElementById(SIDE_BAR_ID);
        if (debuggingPanelExpanded){
            toolPanel.style.width = "0px";
            debuggingPanelExpanded = false;
        } else {
            toolPanel.style.right = "0px";
            toolPanel.style.width = SIDE_BAR_WIDTH + "px";
            debuggingPanelExpanded = true;
        }
    };

    sideBar.appendChild(sideBarTab);
    sideBar.appendChild(sideBarBody);
    return sideBar;
}


// ERROR MESSAGE

var ERROR_MESSAGE_ID = 'elm-reactor-error-message';

function initErrorMessage(message) {
    var node = document.createElement("pre");
    node.id = ERROR_MESSAGE_ID;
    node.innerHTML = message;
    node.style.zindex = 1;
    node.style.position = "absolute";
    node.style.top = "0";
    node.style.left = "0";
    node.style.color = DARK_GREY;
    node.style.backgroundColor = LIGHT_GREY;
    node.style.padding = "1em";
    node.style.margin = "1em";
    node.style.borderRadius = "10px";
    return node;
}


// EVENT BLOCKER

var EVENT_BLOCKER_ID = 'elm-reactor-event-blocker'

var eventsToIgnore = [
    "click", "mousemove", "mouseup", "mousedown", "mouseclick", "keydown",
    "keypress", "keyup", "touchstart", "touchend", "touchcancel", "touchleave",
    "touchmove", "pointermove", "pointerdown", "pointerup", "pointerover",
    "pointerout", "pointerenter", "pointerleave", "pointercancel"
];

function ignore(e) {
    var event = e || window.event;
    if (event.stopPropagation) {
        event.stopPropagation();
    }
    if (event.cancelBubble !== null) {
        event.cancelBubble = true;
    }
    if (event.preventDefault) {
        event.preventDefault();
    }
    return false;
}

function initEventBlocker() {
    var node = document.createElement("div");
    node.id = EVENT_BLOCKER_ID;
    node.style.position = "absolute";
    node.style.top = "0px";
    node.style.left = "0px";
    node.style.width = "100%";
    node.style.height = "100%";

    for (var i = eventsToIgnore.length; i-- ;) {
        node.addEventListener(eventsToIgnore[i], ignore, true);
    }

    return node;
}

function addEventBlocker(node) {
    if (!document.getElementById(EVENT_BLOCKER_ID)) {
        node.appendChild(initEventBlocker());
    }
}

function removeEventBlocker() {
    var blocker = document.getElementById(EVENT_BLOCKER_ID);
    if (blocker)
    {
        blocker.parentNode.removeChild(blocker);
    }
}



// CODE TO SET UP A MODULE FOR DEBUGGING

Elm.fullscreenDebug = function(moduleName, fileName) {
    var result = initModuleWithDebugState(moduleName);

    document.body.appendChild(createSideBar());

    var sideBar = Elm.embed(Elm.SideBar, document.getElementById(SIDE_BAR_BODY_ID), {
        eventCounter: 0,
        watches: [],
        showSwap: true
    });

    function updateWatches(index)
    {
        sideBar.ports.watches.send(watchesAt(index, result.debugState)); 
    }

    sideBar.ports.scrubTo.subscribe(function(index) {
        jumpTo(index, result.debugState);
        updateWatches(index);
    });

    sideBar.ports.pause.subscribe(function(paused) {
        if (paused) {
            pause(result.debugState);
        } else {
            unpause(result.debugState);
            redoTraces(result.debugState);
        }
    });

    sideBar.ports.restart.subscribe(function() {
        restart(result.debugState);
        updateWatches(0);
    });

    sideBar.ports.permitSwap.subscribe(function(permitSwap) {
        result.debugState.permitSwap = permitSwap;
    });

    result.debugState.onNotify = function(debugState) {
        sideBar.ports.eventCounter.send(debugState.index);
        updateWatches(debugState.index);
    };

    // handle swaps
    var updates = 'ws://' + window.location.host + '/socket?file=' + fileName
    var connection = new WebSocket(updates);
    connection.addEventListener('message', function(event) {
        if (result.debugState.permitSwaps)
        {
            result = swap(event.data, result.debugState, result.module.dispose);
            updateWatches(result.debugState.index);
        }
    });
    window.addEventListener("unload", function() {
        connection.close();
    });

    return result.module;
};


function initModuleWithDebugState(moduleName) {
    var debugState;

    function make(localRuntime) {
        var result = initAndWrap(getModule(moduleName), localRuntime);
        debugState = result.debugState;
        return result.values;
    }

    return {
        module: Elm.fullscreen({ make: make }),
        debugState: debugState
    };
}

function getModule(moduleName)
{
    var elmModule = Elm;
    var names = moduleName.split('.');
    for (var i = 0; i < names.length; ++i)
    {
        elmModule = elmModule[names[i]];
    }
    return elmModule;
}


// DEBUG STATE

function emptyDebugState()
{
    return {
        paused: false,
        pausedAtTime: 0,
        totalTimeLost: 0,

        index: 0,
        events: [],
        watches: [{}],
        snapshots: [],
        asyncCallbacks: [],

        initialSnapshot: [],
        initialAsyncCallbacks: [],
        signalGraphNodes: [],

        traces: {},
        traceCanvas: createCanvas(),

        permitSwaps: true,
        swapInProgress: false,

        onNotify: function() {},
        refreshScreen: function() {},
        node: null,
        notify: function() {}
    };
}

function restart(debugState)
{
    var running = !debugState.paused;
    if (running)
    {
        pause(debugState);
    }
    debugState.index = 0;
    debugState.events = [];
    debugState.watches = [debugState.watches[0]];

    var snap = debugState.initialSnapshot;
    debugState.snapshots = [snap];
    for (var i = snap.length; i--; )
    {
        debugState.signalGraphNodes[i].value = snap[i].value;
    }

    debugState.asyncCallbacks = debugState.initialAsyncCallbacks.map(function(thunk) {
        return {
            thunk: thunk,
            id: 0,
            executed: false
        };
    });

    debugState.traces = {};
    redoTraces(debugState);
    debugState.refreshScreen();

    if (running)
    {
        unpause(debugState);
    }
}

function pause(debugState)
{
    if (debugState.paused)
    {
        return;
    }
    debugState.paused = true;
    pauseAsyncCallbacks(debugState);
    debugState.pausedAtTime = Date.now();
    addEventBlocker(debugState.node);
}

function unpause(debugState)
{
    debugState.paused = false;

    // add delay due to the pause itself
    var pauseDelay = Date.now() - debugState.pausedAtTime;
    debugState.totalTimeLost += pauseDelay;

    // add delay if travelling to older event
    if (debugState.index < debugState.events.length - 1)
    {
        debugState.totalTimeLost = Date.now() - debugState.events[debugState.index].time;
    }

    // clear out future snapshots, events, and traces
    var nearestSnapshotIndex = Math.floor(debugState.index / EVENTS_PER_SAVE);
    debugState.snapshots = debugState.snapshots.slice(0, nearestSnapshotIndex + 1);
    debugState.events = debugState.events.slice(0, debugState.index);
    clearTracesAfter(debugState.index, debugState);
    clearWatchesAfter(debugState.index, debugState);

    unpauseAsyncCallbacks(debugState.asyncCallbacks);

    removeEventBlocker();
}

function jumpTo(index, debugState)
{
    if (!debugState.paused)
    {
        pause(debugState);
    }

    assert(
        0 <= index && index <= debugState.events.length,
        "Trying to step to non-existent event index " + index);

    var potentialIndex = indexOfSnapshotBefore(index);
    if (index < debugState.index || potentialIndex > debugState.index)
    {
        var snapshot = getNearestSnapshot(index, debugState.snapshots);

        for (var i = debugState.signalGraphNodes.length; i-- ; )
        {
            debugState.signalGraphNodes[i].value = snapshot[i].value;
        }

        debugState.index = potentialIndex;
    }

    while (debugState.index < index)
    {
        var event = debugState.events[debugState.index];
        debugState.notify(event.id, event.value, event.time);
        debugState.index += 1;
    }
    redoTraces(debugState);
}

function swap(rawJsonResponse, debugState, freeOldRuntime) {
    var error = document.getElementById(ERROR_MESSAGE_ID);
    if (error)
    {
        error.parentNode.removeChild(error);
    }

    var result = JSON.parse(rawJsonResponse);

    if (!result.code)
    {
        var msg = result.error || 'something went wrong with swap';
        document.body.appendChild(initErrorMessage(msg));
        return null;
    }
    // TODO: pause/unpause?
    pauseAsyncCallbacks(debugState);
    window.eval(result.code);

    // remove old nodes
    debugState.node.parentNode.removeChild(debugState.node);
    document.body.removeChild(debugState.traceCanvas);
    freeOldRuntime();

    var result = initModuleWithDebugState(result.name);
    transferState(debugState, result.debugState);
    return result;
}

function transferState(previousDebugState, debugState)
{
    debugState.swapInProgress = true;
    debugState.events = previousDebugState.events;
    debugState.onNotify = previousDebugState.onNotify;

    if (previousDebugState.paused)
    {
        debugState.paused = true;
        pauseAsyncCallbacks(debugState);
        debugState.pausedAtTime = previousDebugState.pausedAtTime;
        debugState.totalTimeLost = previousDebugState.totalTimeLost;
        addEventBlocker(debugState.node);
    }

    while (debugState.index < debugState.events.length)
    {
        var event = debugState.events[debugState.index];
        debugState.index += 1;
        pushWatchFrame(debugState);

        debugState.notify(event.id, event.value, event.time);
        snapshotIfNeeded(debugState);
    }
    redoTraces(debugState);
    debugState.swapInProgress = false;

    jumpTo(previousDebugState.index, debugState);
}


// CALLBACKS

// TODO: is it weird that the callbacks array never shrinks?

function unpauseAsyncCallbacks(callbacks) {
    callbacks.forEach(function(callback) {
        if (!callback.executed) {
            callback.executed = true;
            callback.thunk();
        }
    });
}

function pauseAsyncCallbacks(debugState) {
    debugState.asyncCallbacks.forEach(function(callback) {
        if (!callback.executed) {
            clearTimeout(callback.id);
        }
    });
}



// TRACES

function clearTracesAfter(index, debugState)
{
    var newTraces = {};
    for (var id in debugState.traces)
    {
        var trace = debugState.traces[id];
        for (var i = trace.length; i--; )
        {
            if (trace[i].index < index)
            {
                newTraces[id] = debugState.traces[id].slice(0, i + 1);
                break;
            }
        }
    }
    debugState.traces = newTraces;
}

function createCanvas() {
    var canvas = document.createElement('canvas');
    // TODO: make dimensions adjust based on screen size
    canvas.width = window.innerWidth;
    canvas.height = window.innerHeight;
    canvas.style.position = "absolute";
    canvas.style.top = "0";
    canvas.style.left = "0";
    canvas.style.pointerEvents = "none";
    return canvas;
}

function addTraces(debugState)
{
    var ctx = debugState.traceCanvas.getContext('2d');

    ctx.save();
    for (var id in debugState.traces)
    {
        var points = debugState.traces[id];
        if (points.length < 2)
        {
            continue;
        }
        var lastTracePoint = points[points.length - 1];
        if (lastTracePoint.index < debugState.index - 1)
        {
            continue;
        }
        ctx.beginPath();
        ctx.moveTo(lastTracePoint.x, lastTracePoint.y);
        var secondToLastTracePoint = points[points.length - 2];
        ctx.lineTo(secondToLastTracePoint.x, secondToLastTracePoint.y);

        ctx.lineWidth = 1;
        ctx.strokeStyle = "rgba(50, 50, 50, 0.4)";
        ctx.stroke();
    }
    ctx.restore();
}

function redoTraces(debugState) {
    var ctx = debugState.traceCanvas.getContext('2d');

    // TODO: be more clever about the size of the canvas on resize
    ctx.clearRect(0, 0, debugState.traceCanvas.width, debugState.traceCanvas.height);

    ctx.save();
    for (var id in debugState.traces)
    {
        var points = debugState.traces[id];
        var length = points.length;
        if (length < 2)
        {
            continue;
        }
        ctx.beginPath();
        ctx.lineWidth = 1;
        ctx.moveTo(points[0].x, points[0].y);
        var currentIndex = debugState.index;
        var traceIndex = points[0].index;
        for (var i = 1; traceIndex < currentIndex && i < length; ++i)
        {
            var point = points[i];
            ctx.lineTo(point.x, point.y);
            traceIndex = point.index;
        }
        ctx.strokeStyle = "rgba(50, 50, 50, 0.4)";
        ctx.stroke();

        for (; i < length; ++i)
        {
            var point = points[i];
            ctx.lineTo(point.x, point.y);
            traceIndex = point.index;
        }
        ctx.strokeStyle = "rgba(50, 50, 50, 0.2)";
        ctx.stroke();
    }
    ctx.restore();
}

function makeTraceRecorder(debugState, runtime)
{
    var List = Elm.List.make(runtime);
    var Transform = Elm.Transform2D.make(runtime);

    function crawlElement(element)
    {
        if (debugState.paused && !debugState.swapInProgress)
        {
            return;
        }

        var e = element.element;
        if (!e)
        {
            return;
        }
        if (e.ctor === 'Custom' && e.type === 'Collage')
        {
            var w = element.props.width;
            var h = element.props.height;
            var identity = A6( Transform.matrix, 1, 0, 0, -1, w/2, h/2 );
            return A2(List.map, crawlForm(identity), e.model.forms);
        }
    }

    function crawlForm(matrix)
    {
        return function(form) {
            if (form.form.ctor == "FGroup")
            {
                var scale = form.scale;
                var localMatrix = A6( Transform.matrix, scale, 0, 0, scale, form.x, form.y );

                var theta = form.theta
                if (theta !== 0)
                {
                    localMatrix = A2( Transform.multiply, localMatrix, Transform.rotation(theta) );
                }

                var newMatrix = A2( Transform.multiply, matrix, localMatrix );
                A2(List.map, crawlForm(newMatrix), form.form._1);
            }

            var tag = form.trace;
            if (!tag)
            {
                return;
            }

            var x = matrix[0] * form.x + matrix[1] * form.y + matrix[2];
            var y = matrix[3] * form.x + matrix[4] * form.y + matrix[5];

            if ( !(tag in debugState.traces) )
            {
                debugState.traces[tag] = [{ index: debugState.index, x: x, y: y }];
                return;
            }                

            var trace = debugState.traces[tag];
            var lastPoint = trace[trace.length - 1];
            if (lastPoint.x === x && lastPoint.y === y)
            {
                return;
            }
            trace.push({ index: debugState.index, x: x, y: y });
        }
    }

    return crawlElement;
}


// SNAPSHOTS

var EVENTS_PER_SAVE = 100;

function snapshotIfNeeded(debugState)
{
    if (debugState.index % EVENTS_PER_SAVE === 0)
    {
        debugState.snapshots.push(createSnapshot(debugState.signalGraphNodes));
    }
}

function indexOfSnapshotBefore(index)
{
    return Math.floor(index / EVENTS_PER_SAVE) * EVENTS_PER_SAVE;
}

function getNearestSnapshot(i, snapshots)
{
    var snapshotIndex = Math.floor(i / EVENTS_PER_SAVE);
    assert(
        snapshotIndex < snapshots.length && snapshotIndex >= 0,
        "Trying to access non-existent snapshot (event " + i + ", snapshot " + snapshotIndex + ")");
    return snapshots[snapshotIndex];
}

function createSnapshot(signalGraphNodes)
{
    var nodeValues = [];

    signalGraphNodes.forEach(function(node) {
        nodeValues.push({ value: node.value, id: node.id });
    });

    return nodeValues;
}

function flattenSignalGraph(nodes)
{
    var nodesById = {};

    function addAllToDict(node) {
        nodesById[node.id] = node;
        node.kids.forEach(addAllToDict);
    }
    nodes.forEach(addAllToDict);

    var allNodes = Object.keys(nodesById).sort(compareNumbers).map(function(key) {
        return nodesById[key];
    });
    return allNodes;
}

function compareNumbers(a, b) {
    return a - b;
}


// WRAP THE RUNTIME

function initAndWrap(elmModule, runtime)
{
    var debugState = emptyDebugState();

    // runtime is the prototype of wrappedRuntime
    // so we can access all runtime properties too
    var wrappedRuntime = Object.create(runtime);
    wrappedRuntime.notify = notifyWrapper;
    wrappedRuntime.setTimeout = setTimeoutWrapper;

    // make a copy of the wrappedRuntime
    var assignedPropTracker = Object.create(wrappedRuntime);
    var values = elmModule.make(assignedPropTracker);

    // make sure the signal graph is actually a signal & extract the visual model
    var Signal = Elm.Signal.make(assignedPropTracker);
    if ( !('recv' in values.main) )
    {
        values.main = Signal.constant(values.main);
    }
    A2(Signal.map, makeTraceRecorder(debugState, assignedPropTracker), values.main);

    debugState.refreshScreen = function() {
        var main = values.main
        for (var i = main.kids.length ; i-- ; )
        {
            main.kids[i].recv(runtime.timer.now(), true, main.id);
        }
    };

    // The main module stores imported modules onto the runtime.
    // To ensure only one instance of each module is created,
    // we assign them back on the original runtime object.
    Object.keys(assignedPropTracker).forEach(function(key) {
        runtime[key] = assignedPropTracker[key];
    });

    debugState.signalGraphNodes = flattenSignalGraph(wrappedRuntime.inputs);
    debugState.initialSnapshot = createSnapshot(debugState.signalGraphNodes);
    debugState.snapshots = [debugState.initialSnapshot];
    debugState.initialAsyncCallbacks = debugState.asyncCallbacks.map(function(callback) {
        return callback.thunk;
    });
    debugState.node = runtime.node;
    debugState.notify = runtime.notify;

    // Tracing stuff
    document.body.appendChild(debugState.traceCanvas);

    var replace = Elm.Native.Utils.make(assignedPropTracker).replace;

    runtime.debug = {};
    runtime.debug.trace = function(tag, form) {
        return replace([['trace', tag]], form);
    }
    runtime.debug.watch = function(tag, value)
    {
        if (debugState.paused && !debugState.swapInProgress)
        {
            return;
        }
        var index = debugState.index;
        var numWatches = debugState.watches.length - 1;
        assert(
            index === numWatches,
            'number of watch frames (' + numWatches + ') should match current index (' + index + ')');
        debugState.watches[debugState.index][tag] = value;
    }

    runtime.timer.now = now;
    function now()
    {
        return Date.now() - debugState.totalTimeLost;
    };

    function notifyWrapper(id, value)
    {
        // Ignore all events that occur while the program is paused.
        if (debugState.paused)
        {
            return false;
        }

        // Record the event
        debugState.events.push({ id: id, value: value, time: runtime.timer.now() });
        debugState.index += 1;
        pushWatchFrame(debugState);

        var changed = runtime.notify(id, value);

        snapshotIfNeeded(debugState);
        debugState.onNotify(debugState);
        addTraces(debugState);

        return changed;
    }

    function setTimeoutWrapper(thunk, delay)
    {
        if (debugState.paused)
        {
            return 0;
        }

        var callback = {
            thunk: thunk,
            id: 0,
            executed: false
        };

        callback.id = setTimeout(function() {
            callback.executed = true;
            thunk();
        }, delay);

        debugState.asyncCallbacks.push(callback);
        return callback.id;
    }

    return {
        values: values,
        debugState: debugState
    };
}


// WATCHES

function watchesAt(index, debugState)
{
    var watchSnapshot = [];
    var watches = debugState.watches[index];

    for (var name in watches)
    {
        var value = prettyPrint(watches[name], "  ");
        watchSnapshot.push([ name, value ]);
    }
    return watchSnapshot;
}

function pushWatchFrame(debugState)
{
    var length = debugState.watches.length;
    var oldFrame = length === 0 ? {} : debugState.watches[length - 1];
    var newFrame = {};
    for (var tag in oldFrame)
    {
        newFrame[tag] = oldFrame[tag];
    }
    debugState.watches.push(newFrame);
}

function clearWatchesAfter(index, debugState)
{
    debugState.watches = debugState.watches.slice(0, index + 1);
}

var prettyPrint = function() {

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
