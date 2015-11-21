Elm.Native = Elm.Native || {};
Elm.Native.Reflect = Elm.Native.Reflect || {};
Elm.Native.Reflect.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Reflect = localRuntime.Native.Reflect || {};
	if ('values' in localRuntime.Native.Reflect)
	{
		return localRuntime.Native.Reflect.values;
	}

	function unsafeCast(x)
	{
		return x;
	}

	return localRuntime.Native.Reflect.values = {
		unsafeCast: unsafeCast
	};
};
