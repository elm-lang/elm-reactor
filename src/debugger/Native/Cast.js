Elm.Native = Elm.Native || {};
Elm.Native.Cast = Elm.Native.Cast || {};
Elm.Native.Cast.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Cast = localRuntime.Native.Cast || {};
	if ('values' in localRuntime.Native.Cast)
	{
		return localRuntime.Native.Cast.values;
	}
	var Maybe = Elm.Maybe.make(localRuntime);

	function unsafeCast(x)
	{
		return x;
	}

	function getFunctionName(value)
	{
		if (typeof value === 'function')
		{
			return Maybe.Just(value.name);
		}
		return Maybe.Nothing;
	}

	return localRuntime.Native.Cast.values = {
		unsafeCast: unsafeCast,
		getFunctionName: getFunctionName
	};
};
