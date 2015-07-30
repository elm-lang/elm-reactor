(function() {
'use strict';

function assert(bool, msg)
{
	if (!bool)
	{
		throw new Error("Assertion error: " + msg);
	}
}


// SIDE BAR

var SIDE_BAR_ID = "elm-reactor-side-bar";
var SIDE_BAR_BODY_ID = "elm-reactor-side-bar-body";
var SIDE_BAR_WIDTH = 275;

var DARK_GREY = "#4A4A4A";
var LIGHT_GREY = "#E4E4E4";


// ERROR MESSAGE

var ERROR_MESSAGE_ID = 'elm-reactor-error-message';

function initErrorMessage(message)
{
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

var eventsToIgnore = [
	"click", "mousemove", "mouseup", "mousedown", "mouseclick", "keydown",
	"keypress", "keyup", "touchstart", "touchend", "touchcancel", "touchleave",
	"touchmove", "pointermove", "pointerdown", "pointerup", "pointerover",
	"pointerout", "pointerenter", "pointerleave", "pointercancel"
];

function ignore(e)
{
	var event = e || window.event;
	if (event.stopPropagation)
	{
		event.stopPropagation();
	}
	if (event.cancelBubble !== null)
	{
		event.cancelBubble = true;
	}
	if (event.preventDefault)
	{
		event.preventDefault();
	}
	return false;
}

// attachment of debugger to an elm module
function DebugSession(debuggerModule, moduleBeingDebugged, runtime) {
	this.debuggerModule = debuggerModule;
	this.moduleBeingDebugged = moduleBeingDebugged;
	this.runtime = runtime;
	this.originalNotify = this.runtime.notify;
	this.node = this.runtime.node;

	this.signalGraphNodes = this.flattenSignalGraph();
	this.permitSwap = true;
	this.paused = false;
	this.swapInProgress = false; // TODO: move to elm side
	this.delay = 0;
	this.asyncCallbacks = []; // TODO: think this through; rename to timeouts (?)
	
	console.log(this);

	this.attachOutputs(); // from debugger
	this.attachInputs(); // into debugger
	this.attachFunctions();

	this.watchUpdates = [];

	this.debuggerModule.ports.attachments.send(this.takeSnapshot());
}

// returns list of node references
DebugSession.prototype.flattenSignalGraph = function() {
	var nodesById = {};

	function addAllToDict(node)
	{
		nodesById[node.id] = node;
		if(node.kids) {
			node.kids.forEach(addAllToDict);
		}
	}
	this.runtime.inputs.forEach(addAllToDict);

	var allNodes = Object.keys(nodesById).sort(compareNumbers).map(function(key) {
		return nodesById[key];
	});
	return allNodes;
}

// returns snapshot
DebugSession.prototype.takeSnapshot = function() {
	var nodeValues = [];

	this.signalGraphNodes.forEach(function(node) {
		nodeValues.push({ value: node.value, id: node.id });
	});

	console.log('TAKESNAP', this.prettyPrintSnapshot(nodeValues));

	return nodeValues;
}

DebugSession.prototype.attachOutputs = function() {
	var _this = this;
	var ports = this.debuggerModule.ports;

	ports.captureSnapshot.subscribe(function(_) {
		var snapshot = _this.takeSnapshot();
		_this.debuggerModule.ports.snapshots.send(snapshot);
	});
	
	ports.setToSnapshot.subscribe(function(snapshot) {
		console.log('SETSNAP', _this.prettyPrintSnapshot(snapshot));
		for (var i = _this.signalGraphNodes.length; i-- ; )
		{
		  _this.signalGraphNodes[i].value = snapshot[i].value;
		}
	});
	
	ports.processEvents.subscribe(function(events) {
		for(var i=0; i < events.length; i++)
		{
			var event = events[i];
			_this.originalNotify(event.id, event.value);
		}
	});
	
	ports.delayUpdate.subscribe(function(delay) {
		_this.delay = delay;
	});
	
	ports.permitSwap.subscribe(function(permit) {
		_this.permitSwap = permit;
	});

	ports.paused.subscribe(function(paused) {
		_this.paused = paused;
		console.log('PAUSE', paused)
	});
}

DebugSession.prototype.prettyPrintSnapshot = function(snapshot) {
	var output = [];
	for (var i = this.signalGraphNodes.length; i-- ; )
	{
	  var value = snapshot[i].value;
	  if(value) {
	  	output.push(prettyPrint(value, "  "));
	  } else {
	  	output.push("<<undefined>>");
	  }
	}
	return output;
}


DebugSession.prototype.attachInputs = function() {
	var _this = this;
	this.runtime.notify = function(id, value) {
		if (_this.paused)
		{
			return false;
		}

		_this.watchUpdates = [];

		var changed = _this.originalNotify(id, value);

		// Record the event

		_this.debuggerModule.ports.events.send({
			id: id,
			value: value,
			time: _this.runtime.timer.now(),
			watchUpdate: _this.watchUpdates
		});

		// TODO: add traces

		return changed;
	};

	this.runtime.setTimeout = function(thunk, delay) {
		if (_this.paused)
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

		// TODO: this isn't fully hooked up yet
		_this.asyncCallbacks.push(callback);
		return callback.id;
	};
}

DebugSession.prototype.attachFunctions = function() {
	var _this = this;
	this.runtime.timer.now = function() {
		// TODO: not sure how to get last event
		// if (debugState.paused || debugState.swapInProgress)
		// {
		// 	var event = debugState.events[debugState.index];
		// 	return event.time;
		// }
		return Date.now() - _this.delay;
	};
	this.runtime.debug = {
		watch: function(tag, value) {
			if (_this.paused && !_this.swapInProgress)
			{
				return;
			}
			_this.watchUpdates.push([tag, prettyPrint(value, "  ")]);
		},
		trace: function(tag, form) {
			return replace([['trace', tag]], form);
		}
	};
}

// returns nothing
// unregisters all handlers, destroying connection between
// debugger and module being debugged
// also disposes module being debugged
DebugSession.prototype.dispose = function() {
	// TODO: figure out how to unsubscribe all the ports
	// ports.captureSnapshot.unsubscribe(this.handleCaptureSnapshot);
	// ports.setToSnapshot.unsubscribe(this.handleSetToSnapshot);
	// ports.processEvents.unsubscribe(this.handleProcessEvents);
	// ports.delayUpdate.unsubscribe(this.handleDelayUpdate);
	// ports.permitSwap.unsubscribe(this.handlePermitSwap);
	this.moduleBeingDebugged.dispose();
}

// CODE TO SET UP A MODULE FOR DEBUGGING

Elm.fullscreenDebug = function(moduleName, fileName) {

	// initialize module

	var module = getModule(moduleName);
	var localRuntime;
	var moduleBeingDebugged = Elm.fullscreen({
		make: function(runtime) {
			localRuntime = runtime;
			return module.make(runtime);
		}
	});

	// initialize debugger and its container

	var container = document.createElement("div");
	document.body.appendChild(container);

	var debuggerModule = Elm.embed(Elm.Debugger, container, {
		// NB: these aren't captured on Elm side
		// these should really be streams, not signals
		// initial values are meaningless
		attachments: [],
		snapshots: [],
		events: {id: 0, value: null, time: 0, watchUpdate: []}
	});

	var sideBar = document.getElementById('elm-reactor-side-bar');
	sideBar.addEventListener("click", blockClicks);
	function blockClicks(e)
	{
		var event = e || window.event;
		event.cancelBubble = true;
		if (event.stopPropagation)
		{
			event.stopPropagation();
		}
	}

	var eventBlocker = document.getElementById('elm-reactor-event-blocker');
	for (var i = eventsToIgnore.length; i-- ;)
	{
		eventBlocker.addEventListener(eventsToIgnore[i], ignore, true);
	}

	// attach the two
	var debugSession = new DebugSession(debuggerModule, moduleBeingDebugged, localRuntime);

	// handle swaps
	var updates = 'ws://' + window.location.host + '/socket?file=' + fileName
	var connection = new WebSocket(updates);
	connection.addEventListener('message', function(event) {
		if (debugSession.permitSwaps)
		{
			debugSession.dispose();
			console.log("swap"); // TODO: initialize new mod; make new session
		}
	});
	window.addEventListener("unload", function() {
		connection.close();
	});

	return moduleBeingDebugged;
};


function initModuleWithDebugState(moduleName)
{
	var debugState;

	function make(localRuntime)
	{
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
		asyncCallbacks: [],

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

function swap(rawJsonResponse, oldResult)
{
	var error = document.getElementById(ERROR_MESSAGE_ID);
	if (error)
	{
		error.parentNode.removeChild(error);
	}

	var response = JSON.parse(rawJsonResponse);

	if (!response.code)
	{
		var msg = response.error || 'something went wrong with swap';
		document.body.appendChild(initErrorMessage(msg));
		return oldResult;
	}
	// TODO: pause/unpause?
	pauseAsyncCallbacks(oldResult.debugState);
	window.eval(response.code);

	// remove old nodes
	oldResult.debugState.node.parentNode.removeChild(oldResult.debugState.node);
	document.body.removeChild(oldResult.debugState.traceCanvas);
	oldResult.module.dispose();

	var result = initModuleWithDebugState(response.name);
	transferState(oldResult.debugState, result.debugState);
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
	}

	while (debugState.index < debugState.events.length)
	{
		var event = debugState.events[debugState.index];
		debugState.notify(event.id, event.value);
		debugState.index += 1;
		snapshotIfNeeded(debugState);
	}
	redoTraces(debugState);
	debugState.swapInProgress = false;

	jumpTo(previousDebugState.index, debugState);
}


// CALLBACKS

// TODO: is it weird that the callbacks array never shrinks?

function unpauseAsyncCallbacks(callbacks)
{
	callbacks.forEach(function(callback) {
		if (!callback.executed)
		{
			callback.executed = true;
			callback.thunk();
		}
	});
}

function pauseAsyncCallbacks(debugState)
{
	debugState.asyncCallbacks.forEach(function(callback) {
		if (!callback.executed)
		{
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

	function addAllToDict(node)
	{
		nodesById[node.id] = node;
		node.kids.forEach(addAllToDict);
	}
	nodes.forEach(addAllToDict);

	var allNodes = Object.keys(nodesById).sort(compareNumbers).map(function(key) {
		return nodesById[key];
	});
	return allNodes;
}

function compareNumbers(a, b)
{
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
	if ( !('notify' in values.main) )
	{
		values.main = Signal.constant(values.main);
	}
	A2(Signal.map, makeTraceRecorder(debugState, assignedPropTracker), values.main);

	debugState.refreshScreen = function() {
		var main = values.main
		for (var i = main.kids.length ; i-- ; )
		{
			main.kids[i].notify(runtime.timer.now(), true, main.id);
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
	debugState.initialAsyncCallbacks = debugState.asyncCallbacks.map(function(callback) {
		return callback.thunk;
	});
	debugState.node = runtime.node;
	debugState.notify = runtime.notify;

	// Tracing stuff
	document.body.appendChild(debugState.traceCanvas);

	var replace = Elm.Native.Utils.make(assignedPropTracker).replace;

	runtime.timer.now = function() {
		if (debugState.paused || debugState.swapInProgress)
		{
			var event = debugState.events[debugState.index];
			return event.time;
		}
		return Date.now() - debugState.totalTimeLost;
	};

	runtime.debug = {};

	runtime.debug.trace = function(tag, form) {
		return replace([['trace', tag]], form);
	}

	runtime.debug.watch = function(tag, value) {
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
		if (type === 'object' && 'notify' in v) return '<signal>';
		return "<internal structure>";
	};

	function addSlashes(str)
	{
		return str.replace(/\\/g, '\\\\')
				  .replace(/\n/g, '\\n')
				  .replace(/\t/g, '\\t')
				  .replace(/\r/g, '\\r')
				  .replace(/\v/g, '\\v')
				  .replace(/\0/g, '\\0')
				  .replace(/\'/g, "\\'")
				  .replace(/\"/g, '\\"');
	}

	function probablyPublic(v)
	{
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
