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

	var Utils = Elm.Native.Utils.make (localRuntime);
	var List = Elm.Native.List.make (localRuntime);
	var Dict = Elm.Dict.make (localRuntime);

	function decodeJsArray(value) {
		if(value instanceof Array) {

		}
		throw new Error('expecting list of events; got ')
	}

	// not exposed
	function decodeEvent(value) {

	}

	return localRuntime.Native.Debugger.RuntimeApi.values = {
		decodeJsArray: decodeInputHistory,
		encodeInputHistory: encodeInputHistory,

		splitInputHistory: F2(splitInputHistory),
		splitSnapshots: F2(splitSnapshots),

		timeOfLastEvent: timeOfLastEvent
	};
};
