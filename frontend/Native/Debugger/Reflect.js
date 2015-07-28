Elm.Native = Elm.Native || {};
Elm.Native.Debugger = Elm.Native.Debugger || {};
Elm.Native.Debugger.Reflect = Elm.Native.Debugger.Reflect || {};

Elm.Native.Debugger.Reflect = {};
Elm.Native.Debugger.Reflect.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Debugger = localRuntime.Native.Debugger || {};
	localRuntime.Native.Debugger.Reflect = localRuntime.Native.Debugger.Reflect || {};
	if ('values' in localRuntime.Native.Debugger.Reflect)
	{
		return localRuntime.Native.Debugger.Reflect.values;
	}

	var Signal = Elm.Native.Signal.make (localRuntime);
	var Task = Elm.Native.Task.make (localRuntime);
	var Utils = Elm.Native.Utils.make (localRuntime);
	var List = Elm.Native.List.make (localRuntime);
	var Dict = Elm.Dict.make (localRuntime);

	function getHtml(html)
	{
		// TODO: I hear this is bad (http://webreflection.blogspot.com/2013/03/5-reasons-you-should-avoid-proto.html)
		// but instanceof didn't work for some reason. find a workaround.
		if(html.__proto__.type == "VirtualNode" || html.__proto__.type == "VirtualText") {
			return html;
		} else {
			throw new Error("not html");
		}
	}

	return localRuntime.Native.Debugger.Reflect.values = {
		getHtml: getHtml
	};
};
