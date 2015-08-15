Elm.Native.JsArray = {};
Elm.Native.JsArray.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.JsArray = localRuntime.Native.JsArray || {};
	if (localRuntime.Native.JsArray.values)
	{
		return localRuntime.Native.JsArray.values;
	}
	if ('values' in Elm.Native.JsArray)
	{
		return localRuntime.Native.JsArray.values = Elm.Native.JsArray.values;
	}

	var Utils = Elm.Native.Utils.make(localRuntime);

	var empty = [];
	Object.freeze(empty);

	function fromMutableArray(array)
	{
		var result = array.slice(); // makes a copy
		Object.freeze(result);
		return result;
	}

	function toMutableArray(array)
	{
		return array.slice();
	}

	function decode(decoder)
	{
		return function(value) {
			if (value instanceof Array)
			{
				var result = [];
				for (var item of value)
				{
					result.push(decoder(item));
				}
				return result;
			}
			throw new Exception('not an Array');
		}
	}

	function encode(value)
	{
		return value;
	}

	function length(array)
	{
		return array.length;
	}
	
	function get(idx, array)
	{
		return array[idx];
	}

	function split(idx, array)
	{
		var before = array.slice(0, idx);
		Object.freeze(before);
		var after = array.slice(idx);
		Object.freeze(after);
		return Utils.Tuple2(before, after);
	}

	function map(func, array)
	{
		var result = [];
		for (var item of array)
		{
			result.push(func(item));
		}
		Object.freeze(result);
		return result;
	}


	Elm.Native.JsArray.values = {
		empty: empty,
		fromMutableArray: fromMutableArray,
		toMutableArray: toMutableArray,
		decode: decode,
		encode: encode,
		length: length,
		get: F2(get),
		split: F2(split),
		map: F2(map)
	};
	return localRuntime.Native.JsArray.values = Elm.Native.JsArray.values;

};
