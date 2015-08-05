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

	var VirtualDom = Elm.VirtualDom.make (localRuntime);

	function getHtml(mainVal)
	{
		// TODO: I hear this is bad (http://webreflection.blogspot.com/2013/03/5-reasons-you-should-avoid-proto.html)
		// but instanceof didn't work for some reason. find a workaround.
		if(mainVal.__proto__.type == "VirtualNode" || mainVal.__proto__.type == "VirtualText") {
			return mainVal;
		} else if(mainVal.element && mainVal.props) {
			return VirtualDom.fromElement(mainVal);
		} else {
			throw new Error("not Html or Element");
		}
	}

	return localRuntime.Native.Debugger.Reflect.values = {
		getHtml: getHtml
	};
};
