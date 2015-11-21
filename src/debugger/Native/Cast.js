Elm.Native = Elm.Native || {};
Elm.Native.Cast = Elm.Native.Cast || {};
Elm.Native.Cast.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Cast = localRuntime.Native.Cast || {};
	if ('values' in localRuntime.Native.Cast)
	{
		return localRuntime.Native.Cast.values;
	}

	function unsafeCast(x)
	{
		return x;
	}

	return localRuntime.Native.Cast.values = {
		unsafeCast: unsafeCast
	};
};
