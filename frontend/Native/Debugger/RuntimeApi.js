Elm.Native = Elm.Native || {};
Elm.Native.Debugger = Elm.Native.Debugger || {};
Elm.Native.Debugger.RuntimeApi = Elm.Native.Debugger.RuntimeApi || {};

Elm.Native.Debugger.RuntimeApi = {};
Elm.Native.Debugger.RuntimeApi.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Debugger = localRuntime.Native.Debugger || {};
	localRuntime.Native.Debugger.RuntimeApi = localRuntime.Native.Debugger.RuntimeApi || {};
	if ('values' in localRuntime.Native.Debugger.RuntimeApi)
	{
		return localRuntime.Native.Debugger.RuntimeApi.values;
	}

	var Signal = Elm.Native.Signal.make (localRuntime);
	var Task = Elm.Native.Task.make (localRuntime);
	var Utils = Elm.Native.Utils.make (localRuntime);
	var List = Elm.Native.List.make (localRuntime);
	var Dict = Elm.Dict.make (localRuntime);
	var JsArray = Elm.Native.JsArray.make (localRuntime);

	// not exposed
	function jumpTo(session, frameIdx)
	{
		// get to interval start
		var snapshotBeforeIdx = Math.floor(frameIdx / eventsPerSnapshot);
		var snapshot = session.snapshots[snapshotBeforeIdx];
		for(var nodeId in snapshot) {
			session.sgNodes[nodeId].value = snapshot[nodeId];
		}
		var snapshotBeforeFrameIdx = snapshotBeforeIdx * eventsPerSnapshot;
		for(var idx=snapshotBeforeFrameIdx; idx < frameIdx; idx++)
		{
			var event = session.events[idx];
			session.originalNotify(event.nodeId, event.value);
		}
	}

	// not exposed
	function initializeFullscreen(module, delay, notificationAddress)
	{
		var session;
		var moduleBeingDebugged = Elm.fullscreen({
			make: function(debugeeLocalRuntime) {
				session = {
					module: module,
					runtime: debugeeLocalRuntime,
					originalNotify: debugeeLocalRuntime.notify,
					delay: delay,
					startedAt: Date.now(),
					// TODO: delay, totalTimeLost, asyncCallbacks
					asyncCallbacks: [],
					events: [],
					notificationAddress: notificationAddress,
					disposed: false,
					playing: true,
					replayingHistory: false,
					subscribedNodeIds: [],
					flaggedExprValues: [],
					setIntervalIds: []
				};

				// set up event recording
				debugeeLocalRuntime.notify = function(id, value) {
					if (!session.playing || session.replayingHistory)
					{
						return false;
					}

					session.flaggedExprValues = [];

					var changed = session.originalNotify(id, value);

					// Record the event
					var event = {
						_: {},
						value: value,
						nodeId: id,
						time: session.runtime.timer.now()
					}
					session.events.push(event);
					// take snapshot if necessary
					if(session.events.length % eventsPerSnapshot == 0)
					{
						session.snapshots.push(takeSnapshot(session.sgNodes));
					}
					
					var subscribedNodeValues = session.subscribedNodeIds.map(function(nodeId) {
						var node = session.sgNodes[nodeId];
						return Utils.Tuple2(nodeId, node.value);
					});
					// send notification
					var notification = {
						_: {},
						event: event,
						flaggedExprValues: List.fromArray(session.flaggedExprValues),
						subscribedNodeValues: List.fromArray(subscribedNodeValues)
					}
					Task.perform(notificationAddress._0(notification));

					// TODO: add traces

					return changed;
				};

				debugeeLocalRuntime.setTimeout = function(thunk, delay) {
					if (!session.playing)
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
						console.log("thunk", session.id);
						thunk();
					}, delay);

					// TODO: this isn't fully hooked up yet
					session.asyncCallbacks.push(callback);
					return callback.id;
				};

				debugeeLocalRuntime.setInterval = function(thunk, interval) {
					var intervalId = window.setInterval(function() {
						if(session.playing && !session.replayingHistory)
						{
							thunk();
						}
					}, interval);
					session.setIntervalIds.push(intervalId);
				}

				debugeeLocalRuntime.timer.now = function() {
					// TODO: not sure how to get time of last event
					// if (debugState.paused || debugState.swapInProgress)
					// {
					// 	var event = debugState.events[debugState.index];
					// 	return event.time;
					// }
					return Date.now() - session.delay;
				};
				debugeeLocalRuntime.debug = {
					log: function(tag, value) {
						if (!session.playing)
						{
							return;
						}
						session.flaggedExprValues.push(Utils.Tuple2(tag, value));
					},
					trace: function(tag, form) {
						// TODO: ...
						return replace([['trace', tag]], form);
					}
				};

				return module.modul.make(debugeeLocalRuntime);
			}
		}, {});

		session.runningModule = moduleBeingDebugged;
		session.sgNodes = flattenSignalGraph(session.runtime);
		session.shape = extractSgShape(session.sgNodes);
		session.snapshots = [takeSnapshot(session.sgNodes)];

		return session;
	}

	function start(module, address)
	{
		return Task.asyncFunction(function(callback) {
			var session = initializeFullscreen(module, 0, address);
			callback(Task.succeed(session));
		})
	}

	function swap(module, address, inputHistory, maybeShape, validator)
	{
		return Task.asyncFunction(function(callback) {
			var session = initializeFullscreen(module, 0, address);
			// TODO: basically writing Elm in JS here. should do it in Elm.
			var valid;
			switch (maybeShape.ctor) {
				case 'Just':
					valid = A3(validator, maybeShape._0, session.shape, inputHistory);
					break;

				case 'Nothing':
					valid = true;
					break;
			}
			if (!valid) {
				var error = {
					_: {},
					oldShape: maybeShape._0,
					newShape: session.shape
				}
				callback(Task.fail(error));
			} else {
				session.replayingHistory = true;

				session.events = JsArray.toMutableArray(inputHistory);
				
				var flaggedExprLogs = {};
				var nodeLogs = {};
				session.subscribedNodeIds.forEach(function(nodeId) {
					var value = session.sgNodes[nodeId].value;
					nodeLogs[nodeId] = [Utils.Tuple2(0, value)];
				});

				// replay events, regenerating snapshots and capturing
				// flagged expr logs along the way
				for (var i = 0; i < session.events.length; i++)
				{
					var event = session.events[i];

					session.flaggedExprValues = [];
					session.originalNotify(event.nodeId, event.value);
					var frameIdx = i + 1;
					session.flaggedExprValues.forEach(function(tagAndVal) {
						var tag = tagAndVal._0;
						var value = tagAndVal._1;
						if (!(tag in flaggedExprLogs)) {
							flaggedExprLogs[tag] = [];
						}
						flaggedExprLogs[tag].push(Utils.Tuple2(frameIdx, value));
					});
					session.subscribedNodeIds.forEach(function(nodeId) {
						var value = session.sgNodes[nodeId].value;
						nodeLogs[nodeId].push(Utils.Tuple2(frameIdx, value));
					});

					if(i != 0 && i % eventsPerSnapshot == 0)
					{
						session.snapshots.push(takeSnapshot(session.sgNodes));
					}
				}
				var flaggedExprLogsArray = [];
				for (var exprTag in flaggedExprLogs)
				{
					var log = List.fromArray(flaggedExprLogs[exprTag]);
					flaggedExprLogsArray.push(Utils.Tuple2(exprTag, log));
				}
				var flaggedExprLogsList = List.fromArray(flaggedExprLogsArray);

				var nodeLogsArray = [];
				for (var nodeId of session.subscribedNodeIds)
				{
					var log = List.fromArray(nodeLogs[nodeId]);
					nodeLogsArray.push(Utils.Tuple2(nodeId, log));
				}
				var nodeLogsList = List.fromArray(nodeLogsArray);

				session.replayingHistory = false;

				var result = {
					ctor: '_Tuple3',
					_0: session,
					_1: flaggedExprLogsList,
					_2: nodeLogsList
				};

				callback(Task.succeed(result));
			}
		});
	}

	function play(record, address)
	{
		return Task.asyncFunction(function (callback) {
			var timePaused = Date.now() - record.pausedAt;
			var delay = record.delay + timePaused;
			var session = initializeFullscreen(record.modul, delay, address);
			session.events = JsArray.toMutableArray(record.inputHistory);
			session.snapshots = JsArray.toMutableArray(record.snapshots);
			callback(Task.succeed(session));
		});
	}

	function pause(session)
	{
		return Task.asyncFunction(function(callback) {
			assertNotDisposed(session, callback, function() {
				if(session.playing) {

					session.playing = false;

					var sessionRecord = {
						_: {},
						sgShape: session.shape,
						modul: session.module,
						startedAt: session.startedAt,
						pausedAt: Date.now(),
						delay: session.delay,
						snapshots: JsArray.fromMutableArray(session.snapshots),
						inputHistory: JsArray.fromMutableArray(session.events)
					};

					callback(Task.succeed(sessionRecord));
				} else {
					callback(Task.fail(Utils.Tuple0));
				}
			});
		});
	}

	function dispose(session)
	{
		return Task.asyncFunction(function(callback) {
			session.disposed = true;
			session.runningModule.dispose();
			for (var intervalId of session.setIntervalIds)
			{
				window.clearInterval(intervalId);
			}
			session.runtime.node.parentNode.removeChild(session.runtime.node);
			callback(Task.succeed(Utils.Tuple0));
		});
	}

	function setSubscribedToNode(session, nodeId, subscribed)
	{
		return Task.asyncFunction(function(callback) {
			assertNotDisposed(session, callback, function() {
				var idx = session.subscribedNodeIds.indexOf(nodeId);
				var alreadySubscribed = idx != -1;
				if(subscribed) {
					if(alreadySubscribed) {
						callback(Task.fail(Utils.Tuple0));
					} else {
						session.subscribedNodeIds.push(nodeId);
						callback(Task.succeed(Utils.Tuple0));
					}
				} else {
					if(alreadySubscribed) {
						session.subscribedNodeIds.splice(idx, 1);
						callback(Task.succeed(Utils.Tuple0));
					} else {
						callback(Task.fail(Utils.Tuple0));
					}
				}
			});
		});
	}

	function renderMain(session, mainValue)
	{
		return Task.asyncFunction(function(callback) {
			var mainNode = session.sgNodes[session.shape.mainId];
			mainNode.value = mainValue;
			mainNode.notify(session.runtime.timer.now(), true, mainNode.parents[0].id);
			callback(Task.succeed(Utils.Tuple0));
		});
	}

	function getSgShape(session)
	{
		return session.shape;
	}

	function getModule(session)
	{
		return session.module;
	}

	function getAddress(session)
	{
		return session.notificationAddress;
	}

	function getSubscriptions(session)
	{
		return Task.asyncFunction(function(callback) {
			callback(Task.succeed(List.fromArray(session.subscribedNodeIds)));
		});
	}

	function getNodeState(session, frameInterval, nodeIds)
	{
		return Task.asyncFunction(function(callback) {
			assertNotDisposed(session, callback, function() {
				assertPaused(session, callback, function() {
					assertIntervalInRange(session, frameInterval, callback, function() {
						nodeIds = List.toArray(nodeIds);

						jumpTo(session, frameInterval.start);

						// go through the target range, collecting node value logs
						var valueLogs = {};
						nodeIds.forEach(function(nodeId) {
							valueLogs[nodeId] = [];
						});
						for(var idx = frameInterval.start; idx <= frameInterval.end; idx++)
						{
							// get values
							nodeIds.forEach(function(nodeId) {
								var tuple = Utils.Tuple2(idx, session.sgNodes[nodeId].value);
								valueLogs[nodeId].push(tuple);
							});
							// push event
							if(idx < frameInterval.end)
							{
								var event = session.events[idx];
								session.originalNotify(event.nodeId, event.value);
							}
						}

						var logs = nodeIds.map(function(nodeId) {
							return Utils.Tuple2(nodeId, List.fromArray(valueLogs[nodeId]));
						});

						callback(Task.succeed(List.fromArray(logs)));
					});
				});
			});
		});
	}

	function getSessionRecord(session)
	{
		return Task.asyncFunction(function(callback) {
			var result = {
				_: {},
				moduleName: session.module.name,
				inputHistory: JsArray.fromMutableArray(session.events)
			}
			callback(Task.succeed(result));
		});
	}

	// GETTING MODULE FROM JS

	function getFromGlobalScope(moduleName)
	{
		return Task.asyncFunction(function(callback) {
			var elmModule = Elm;
			var names = moduleName.split('.');
			try {
				for (var i = 0; i < names.length; ++i)
				{
					elmModule = elmModule[names[i]];
				}
			} catch (err) {
				callback(Task.fail(err.message));
			}
			var result = {
				_: {},
				modul: elmModule,
				name: moduleName
			};
			callback(Task.succeed(result));
		});
	}

	function evalCompiledModule(compiledModule)
	{
		return Task.asyncFunction(function(callback) {
			window.eval(compiledModule.code);
			callback(Task.succeed(Utils.Tuple0));
		});
	}

	// == NOT EXPOSED BELOW HERE ==

	// Bool -> a -> (Task -> ()) -> (() -> ()) -> ()
	function assert(bool, err, callback, thunk)
	{
		if(!bool) {
			callback(Task.fail(err));
		} else {
			thunk();
		}
	}

	function assertNotDisposed(session, callback, thunk)
	{
		assert(!session.disposed, {ctor: "IsDisposed"}, callback, thunk);
	}

	function assertPaused(session, callback, thunk)
	{
		assert(!session.disposed, {ctor: "IsPlaying"}, callback, thunk);
	}

	function assertIntervalInRange(session, interval, callback, thunk)
	{
		assert(
			(interval.start >= 0 && interval.start <= session.events.length)
				&& (interval.end >= 0 && interval.end <= session.events.length),
			{ctor: "EventIndexOutOfRange", _0: interval},
			callback,
			thunk
		);
	}

	var eventsPerSnapshot = 100;

	function extractSgShape(nodes)
	{
		var mainId;
		var nodeTuples = Object.keys(nodes).map(function(nodeId) {
			var node = nodes[nodeId];
			var nodeType;
			if(node.name == 'input-mailbox') {
				nodeType = {ctor: 'Mailbox'};
			} else if(node.name.indexOf('input') == 0) {
				nodeType = {ctor: 'CoreLibInput'};
			} else if(node.isOutput && node.isOutput) {
				if(node.name == 'output-main') {
					nodeType = {ctor:'Main'};
					mainId = node.id;
				} else {
					nodeType = {ctor:'OutputPort'}
				}
			} else {
				nodeType = {ctor: 'InternalNode'};
			}
			var info = {
				_: {},
				name: node.name,
				nodeType: nodeType,
				kids: List.fromArray(
					node.kids ? node.kids.map(function(kid) {return kid.id}) : []
				)
			};
			return Utils.Tuple2(node.id, info);
		});
		return {
			_: {},
			nodes: Dict.fromList(List.fromArray(nodeTuples)),
			mainId: mainId
		}
	}

	// returns array of node references, indexed by node id (?)
	function flattenSignalGraph(runtime) {
		var nodesById = {};

		function addAllToDict(node)
		{
			nodesById[node.id] = node;
			if(node.kids) {
				node.kids.forEach(addAllToDict);
			}
		}
		runtime.inputs.forEach(addAllToDict);

		return nodesById;
	}


	// returns snapshot
	function takeSnapshot(signalGraphNodes)
	{
		var nodeValues = {};

		Object.keys(signalGraphNodes).forEach(function(nodeId) {
			var node = signalGraphNodes[nodeId];
			nodeValues[nodeId] = node.value;
		});

		return nodeValues;
	}

	var prettyPrint = function() {

		var independentRuntime = {};
		var List;
		var ElmArray;
		var Dict;

		var toString = function(v, separator) {
			if (v == null) {
				return "<null>";
			}
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


	return localRuntime.Native.Debugger.RuntimeApi.values = {

		start: F2(start),
		swap: F5(swap),
		play: F2(play),
		pause: pause,
		dispose: dispose,
		setSubscribedToNode: F3(setSubscribedToNode),
		renderMain: F2(renderMain),
		
		getSgShape: getSgShape,
		getModule: getModule,
		getAddress: getAddress,
		getSubscriptions: getSubscriptions,
		getNodeState: F3(getNodeState),
		getSessionRecord: getSessionRecord,
		
		getFromGlobalScope: getFromGlobalScope,
		evalCompiledModule: evalCompiledModule,

		eventsPerSnapshot: eventsPerSnapshot,

		prettyPrint: F2(prettyPrint)
	};
};
