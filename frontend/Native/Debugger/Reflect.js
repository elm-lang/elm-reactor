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

	function getHtml(elmVal)
	{
		// TODO: I hear this is bad (http://webreflection.blogspot.com/2013/03/5-reasons-you-should-avoid-proto.html)
		// but instanceof didn't work for some reason. find a workaround.
		if(elmVal.__proto__.type == "VirtualNode" || elmVal.__proto__.type == "VirtualText") {
			console.log(elmVal);
			return elmVal;
		} else if(elmVal.element && elmVal.props) {
			return VirtualDom.fromElement(elmVal);
		} else {
			throw new Error("not Html or Element");
		}
	}

	return localRuntime.Native.Debugger.Reflect.values = {
		getHtml: getHtml
	};
};
