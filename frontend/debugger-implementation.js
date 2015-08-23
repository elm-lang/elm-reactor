(function() {
'use strict';

var eventsToIgnore = [
   "click", "mousemove", "mouseup", "mousedown", "mouseclick", "keydown",
   "keypress", "keyup", "touchstart", "touchend", "touchcancel", "touchleave",
   "touchmove", "pointermove", "pointerdown", "pointerup", "pointerover",
   "pointerout", "pointerenter", "pointerleave", "pointercancel"
];

// CODE TO SET UP A MODULE FOR DEBUGGING

Elm.fullscreenDebug = function(moduleName, fileName) {

	var debuggerWindow = window.open('/debugger.html');

	window.MODULE_INFO = {
		moduleName: moduleName,
		fileName: fileName
	}

	// ignore events on event blocker
	// var eventBlocker = document.getElementById('elm-reactor-event-blocker');
	// for (var eventName of eventsToIgnore)
	// {
	// 	eventBlocker.addEventListener(eventName, ignore, true);
	// }

};

})();
