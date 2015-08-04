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

Elm.fullscreenDebug = function(module, fileName) {
	Elm.fullscreen(Elm.Debugger, {
		initMod: module,
		fileName: fileName,
		windowLocationHost: window.location.host
	});
};

})();