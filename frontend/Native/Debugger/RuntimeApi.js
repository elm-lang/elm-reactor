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
	var Show = Elm.Native.Show.make (localRuntime);

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
		for(session.index=snapshotBeforeFrameIdx; session.index < frameIdx; session.index++)
		{
			var event = session.events[session.index];
			session.originalNotify(event.nodeId, event.value);
		}
	}

	var latestSessionId = 0;

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
					events: [],
					notificationAddress: notificationAddress,
					disposed: false,
					playing: true,
					subscribedNodeIds: [],
					flaggedExprValues: [],
					setIntervalIds: [],
					index: 0,
					record: null,
					sessionId: latestSessionId++
				};

				// set up event recording
				debugeeLocalRuntime.notify = function(id, value) {
					if (!session.playing)
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
					session.index++;
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

					var id = setTimeout(function() {
						if (session.playing)
						{
							console.log("thunk:", session.sessionId);
							thunk();
						}
					}, delay);
					return id;
				};

				debugeeLocalRuntime.setInterval = function(thunk, interval) {
					var intervalId = window.setInterval(function() {
						if(session.playing)
						{
							thunk();
						}
					}, interval);
					session.setIntervalIds.push(intervalId);
				}

				debugeeLocalRuntime.timer.now = function() {
					if (!session.playing)
					{
						var t =
							session.index < session.events.length
								? session.events[session.index].time
								: session.record.pausedAt
								;
						return t;
						
					}
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

	function start(module, address, subscribedNodesFun)
	{
		return Task.asyncFunction(function(callback) {
			var session = initializeFullscreen(module, 0, address);
			session.subscribedNodeIds =
				List.toArray(subscribedNodesFun(session.shape));
			var values = session.subscribedNodeIds.map(function(nodeId) {
				return Utils.Tuple2(nodeId, session.sgNodes[nodeId].value);
			});
			var result = Utils.Tuple2(session, List.fromArray(values));
			callback(Task.succeed(result));
		})
	}

	function swap(module, address, subscribedNodesFun, inputHistory, maybeShape, validator)
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

				session.events = JsArray.toMutableArray(inputHistory);
				session.subscribedNodeIds =
					List.toArray(subscribedNodesFun(session.shape));
				session.playing = false;
				
				var flaggedExprLogs = {};
				var nodeLogs = {};
				session.subscribedNodeIds.forEach(function(nodeId) {
					var value = session.sgNodes[nodeId].value;
					nodeLogs[nodeId] = [Utils.Tuple2(0, value)];
				});

				// replay events, regenerating snapshots and capturing
				// flagged expr logs along the way
				for (; session.index < session.events.length; session.index++)
				{
					var event = session.events[session.index];

					session.flaggedExprValues = [];
					session.originalNotify(event.nodeId, event.value);
					session.index++;
					var frameIdx = session.index + 1;
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

					if(session.index != 0 && session.index % eventsPerSnapshot == 0)
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

				var result = {
					ctor: '_Tuple3',
					_0: session,
					_1: flaggedExprLogsList,
					_2: nodeLogsList
				};

				session.playing = true;

				callback(Task.succeed(result));
			}
		});
	}

	function play(record, address, subscribedNodesFun)
	{
		return Task.asyncFunction(function (callback) {
			var timePaused = Date.now() - record.pausedAt;
			var delay = record.delay + timePaused;
			var session = initializeFullscreen(record.modul, delay, address);
			session.events = JsArray.toMutableArray(record.inputHistory);
			session.snapshots = JsArray.toMutableArray(record.snapshots);
			session.playing = false;
			jumpTo(session, session.events.length);
			session.playing = true;
			session.subscribedNodeIds =
				List.toArray(subscribedNodesFun(session.shape));
			var values = session.subscribedNodeIds.map(function(nodeId) {
				return Utils.Tuple2(nodeId, session.sgNodes[nodeId].value);
			});
			var result = Utils.Tuple2(session, List.fromArray(values));
			callback(Task.succeed(result));
		});
	}

	function pause(session)
	{
		return Task.asyncFunction(function(callback) {
			assertNotDisposed(session, callback, function() {
				if(session.playing) {

					session.playing = false;

					session.record = {
						_: {},
						sgShape: session.shape,
						modul: session.module,
						startedAt: session.startedAt,
						pausedAt: Date.now(),
						delay: session.delay,
						snapshots: JsArray.fromMutableArray(session.snapshots),
						inputHistory: JsArray.fromMutableArray(session.events)
					};

					callback(Task.succeed(session.record));
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
						for(; session.index <= frameInterval.end; session.index++)
						{
							// get values
							nodeIds.forEach(function(nodeId) {
								var tuple = Utils.Tuple2(session.index, session.sgNodes[nodeId].value);
								valueLogs[nodeId].push(tuple);
							});
							// push event
							if(session.index < frameInterval.end)
							{
								var event = session.events[session.index];
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
		assert(!session.playing, {ctor: "IsPlaying"}, callback, thunk);
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

	function prettyPrint(value)
	{
		return Show.toString(value);
	}


	return localRuntime.Native.Debugger.RuntimeApi.values = {

		start: F3(start),
		swap: F6(swap),
		play: F3(play),
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

		prettyPrint: prettyPrint
	};
};
