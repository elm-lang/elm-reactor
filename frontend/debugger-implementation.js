(function() {
'use strict';

function assert(bool, msg)
{
	if (!bool)
	{
		throw new Error("Assertion error: " + msg);
	}
}


// SIDE BAR

var SIDE_BAR_ID = "elm-reactor-side-bar";
var SIDE_BAR_BODY_ID = "elm-reactor-side-bar-body";
var SIDE_BAR_WIDTH = 275;

var DARK_GREY = "#4A4A4A";
var LIGHT_GREY = "#E4E4E4";


// ERROR MESSAGE

var ERROR_MESSAGE_ID = 'elm-reactor-error-message';

function initErrorMessage(message)
{
	var node = document.createElement("pre");
	node.id = ERROR_MESSAGE_ID;
	node.innerHTML = message;
	node.style.zindex = 1;
	node.style.position = "absolute";
	node.style.top = "0";
	node.style.left = "0";
	node.style.color = DARK_GREY;
	node.style.backgroundColor = LIGHT_GREY;
	node.style.padding = "1em";
	node.style.margin = "1em";
	node.style.borderRadius = "10px";
	return node;
}


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