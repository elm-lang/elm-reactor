
Elm.Debugger = function() {

	var PrivateDebugger = Elm.Debugger;

	function fullscreen(flags)
	{
		setupPorts(flags, PrivateDebugger.fullscreen());
		return {};
	}

	function embed(flags)
	{
		setupPorts(flags, PrivateDebugger.embed());
		return {};
	}

	function setupPorts(flags, app)
	{
		setupSocket('ws://' + window.location.host + '/_changes/' + flags.file, function(message) {
			var result = JSON.parse(message);

			if ('error' in result)
			{
				app.ports.changes.send(result.error);
				return;
			}

			eval(result.code);
			app.ports.changes.send(elm_reactor_hook);
		});


		/* SCROLLING

		app.ports.scroll.subscribe(function(selector)
		{
			var log = document.querySelector(selector);

			requestAnimationFrame(function() {
				log.scrollTop = log.scrollHeight - log.clientHeight;
			});
		});

		app.ports.askIfScrolled.subscribe(function(selector)
		{
			var log = document.querySelector(selector);
			var isAtEnd = log.scrollTop === log.scrollHeight - log.clientHeight;

			app.ports.tellIfScrolled.send(isAtEnd);
		});

		*/
	}

	function setupSocket(url, callback, backoff)
	{
		var socket = new WebSocket(url);

		socket.addEventListener("message", function(event) {
			callback(event.data);
		});

		socket.addEventListener("open", function(event) {
			backoff = undefined;
		});

		socket.addEventListener("close", function(event) {
			backoff = (backoff || 1) * 2;
			setTimeout(function() {
				setupSocket(url, callback, backoff);
			}, backoff);
		});
	}

	return {
		fullscreen: fullscreen,
		embed: embed
	};

}();