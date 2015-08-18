(function() {
'use strict';

// EVENT BLOCKER

var eventsToIgnore = [
	"click", "mousemove", "mouseup", "mousedown", "mouseclick", "keydown",
	"keypress", "keyup", "touchstart", "touchend", "touchcancel", "touchleave",
	"touchmove", "pointermove", "pointerdown", "pointerup", "pointerover",
	"pointerout", "pointerenter", "pointerleave", "pointercancel"
];

function ignore(e)
{
	var event = e || window.event;
	if (event.stopPropagation)
	{
		event.stopPropagation();
	}
	if (event.cancelBubble !== null)
	{
		event.cancelBubble = true;
	}
	if (event.preventDefault)
	{
		event.preventDefault();
	}
	return false;
}


// CODE TO SET UP A MODULE FOR DEBUGGING

Elm.fullscreenDebug = function(moduleName, fileName) {
	var debugContainer = document.createElement('div');
	document.body.appendChild(debugContainer);
	Elm.embed(Elm.Debugger, debugContainer, {
		moduleName: moduleName,
		fileName: fileName,
		windowLocationHost: window.location.host
	});
	
	// ignore clicks on sidebar
	var debuggerSidebar = document.getElementById('elm-reactor-side-bar');
	debuggerSidebar.addEventListener("click", blockClicks);
	function blockClicks(e)
	{
		var event = e || window.event;
		event.cancelBubble = true;
		if (event.stopPropagation)
		{
			event.stopPropagation();
		}
	}

	// ignore events on event blocker
	var eventBlocker = document.getElementById('elm-reactor-event-blocker');
	for (var eventName of eventsToIgnore)
	{
		eventBlocker.addEventListener(eventName, ignore, true);
	}

};

})();
