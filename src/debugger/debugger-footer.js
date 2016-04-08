
Elm.Debugger = function() {

	var PrivateDebugger = Elm.Debugger;

	function replaceForeigns(app)
	{
		var privateForeign = app.foreign;

		privateForeign.scroll.subscribe(function(selector)
		{
			var log = document.querySelector(selector);

			requestAnimationFrame(function() {
				log.scrollTop = log.scrollHeight - log.clientHeight;
			});
		});

		privateForeign.askIfScrolled.subscribe(function(selector)
		{
			var log = document.querySelector(selector);
			var isAtEnd = log.scrollTop === log.scrollHeight - log.clientHeight;

			privateForeign.tellIfScrolled.send(isAtEnd);
		});

		var newApp = {};
		for (var key in app)
		{
			if (key === 'foreign')
			{
				continue;
			}
			newApp[key] = app[key];
		}
		return newApp;
	}

	function fullscreen()
	{
		return replaceForeigns(PrivateDebugger.fullscreen.apply(this, arguments));
	}

	function embed()
	{
		return replaceForeigns(PrivateDebugger.embed.apply(this, arguments));
	}

	return {
		fullscreen: fullscreen,
		embed: embed
	};

}();