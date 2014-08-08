var elmDebugger = {
    restart: function() {},
    pause: function() {},
    kontinue: function() {},
    getMaxSteps: function() { return 0; },
    stepTo: function(i) {},
    getPaused: function() { return false; },
    getHotSwapState: function() { return null; },
    watchTracker: { frames:[{}] }
};

var mainHandle = {};
var debuggerHandle = {};
var createdSocket = false;
var filePath;

var elmPauseState = false;
var elmPermitHotswaps = true;

var ELM_MAIN_ID = "elmMain";
var ELM_DEBUGGER_ID = "elmToolPanel";
var ELM_DARK_GREY = "#4A4A4A";
var ELM_LIGHT_GREY = "#E4E4E4";

function createMainElement() {
    var mainDiv = document.createElement("div");
    mainDiv.id = ELM_MAIN_ID;
    mainDiv.style.width = "100%";
    mainDiv.style.height = "100%";
    return mainDiv;
}

function createDebuggingElement() {
    var debuggingPanelExpanded = true;
    var debuggerWidth = 275;

    var debugTools = document.createElement("div");
    debugTools.id = ELM_DEBUGGER_ID;

    var debuggerDiv = document.createElement("div");
    debuggerDiv.id = "elmDebugger";
    debuggerDiv.style.overflow = "hidden";

    // Create and style the panel
    debugTools.style.background = ELM_DARK_GREY;
    debugTools.style.width = debuggerWidth + "px";
    debugTools.style.height = "100%";
    debugTools.style.position = "absolute";
    debugTools.style.top = "0px";
    debugTools.style.left = window.innerWidth - debuggerWidth + "px";
    debugTools.style.transitionDuration = "0.3s";
    debugTools.style.opacity = 0.97;
    debugTools.style.zIndex = 1;

    // Create and style the button
    var debugTab = document.createElement("div");
    debugTab.id = "debugtoggle";
    debugTab.style.position = "absolute";
    debugTab.style.width = "15px";
    debugTab.style.height = "30px";
    debugTab.style.top = window.innerHeight / 2 + "px";
    debugTab.style.left = "-15px";
    debugTab.style.borderTopLeftRadius = "3px";
    debugTab.style.borderBottomLeftRadius = "3px";
    debugTab.style.background = ELM_DARK_GREY;

    // Wire the button
    debugTab.onclick = function() {
        var toolPanel = document.getElementById("elmToolPanel");
        if (debuggingPanelExpanded){
            toolPanel.style.left = window.innerWidth + "px";
            toolPanel.style.width = "0px";
            debuggingPanelExpanded = false;
        } else {
            toolPanel.style.left = window.innerWidth - debuggerWidth + "px";
            toolPanel.style.width = debuggerWidth + "px";
            debuggingPanelExpanded = true;
        }
    }

    debugTools.appendChild(debugTab);
    debugTools.appendChild(debuggerDiv);
    return debugTools;
}




Elm.debugFullscreen = function(module, moduleFile, hotSwapState /* =undefined */) {
    filePath = moduleFile;
    initDebugger();
    mainHandle = Elm.fullscreen(Elm.debuggerAttach(module, hotSwapState));
    return mainHandle;
}

function initDebugger() {
    function scrubber(position) {
        if (elmDebugger.getPaused()) {
            elmDebugger.stepTo(position);
            sendWatches(position);
        }
    }

    function elmPauser(doPause) {
        if (doPause) {
          elmDebugger.pause();
        } else {
            elmDebugger.kontinue();
        }
        elmPauseState = doPause;
    }

    function elmRestart() {
        elmDebugger.restart();
        sendWatches(0);
    }

    function elmHotswap(permitHotswaps) {
        elmPermitHotswaps = permitHotswaps;
    }

    var debugTools = createDebuggingElement();
    document.body.appendChild(debugTools);
    var debuggerDiv = document.getElementById("elmDebugger");

    debuggerHandle = Elm.embed(Elm.DebuggerInterface, debuggerDiv,
        { eventCounter: 0,
          watches: ""
        });
    debuggerHandle.ports.scrubTo.subscribe(scrubber);
    debuggerHandle.ports.pause.subscribe(elmPauser);
    debuggerHandle.ports.restart.subscribe(elmRestart);
    debuggerHandle.ports.permitHotswap.subscribe(elmHotswap);
}

parent.window.addEventListener("message", function(e) {
    if (e.data === "elmDebuggerInit") {
        elmDebugger = parent.Elm.Debugger;
        if (!createdSocket) {
            initSocket();
        }
    } else if (e.data === "elmNotify") {
        var currentPosition = elmDebugger.getMaxSteps();
        debuggerHandle.ports.eventCounter.send(currentPosition);
        sendWatches(currentPosition);
    }
}, false);


function sendWatches(position) {
    function censor(key, value) {
        if (key === "_") {
            return undefined;
        }
        return value;
    }

    var watchAtPoint = elmDebugger.watchTracker.frames[position];
    var jsonWatch = JSON.stringify(watchAtPoint, censor, "  ");
    // Thanks http://stackoverflow.com/questions/11233498/json-stringify-without-quotes-on-properties
    jsonWatch.replace(/\\"/g,"\uFFFF"); // U+FFFF
    jsonWatch = jsonWatch.replace(/\"([^"]+)\":/g,"$1:").replace(/\uFFFF/g,"\\\"");
    jsonWatch = jsonWatch.substr(2, jsonWatch.length - 4); // Removes outer {}
    debuggerHandle.ports.watches.send(jsonWatch);
}

function initSocket() {
    createdSocket = true;
    // "/todo.html" => "todo.elm"
    filePath = filePath || window.location.pathname.substr(1).split(".")[0] + ".elm";
    var socketLocation = "ws://" + window.location.host + "/socket?file=" + filePath;
    var serverConnection = new WebSocket(socketLocation);
    serverConnection.onmessage = function(event) {
        if (elmPermitHotswaps) {
            hotSwap(event.data);
        }
    };
}

function hotSwap(raw) {
    var debuggerDiv = document.getElementById(ELM_DEBUGGER_ID);
    var result = JSON.parse(raw);
    var js = result.success;
    var errorMessage = result.error;
    var error = document.getElementById('ErrorMessage');
    if (error) {
        error.parentNode.removeChild(error);
    }
    if (js) {
        var error = document.getElementById('ErrorMessage');
        if (error) {
            error.parentNode.removeChild(error);
        }
        window.eval(js);
        var moduleStr = js.match(/(Elm\..+)\ =\ \1/)[1];
        var module = window.eval(moduleStr);
        if (Elm.Debugger) {
            var debuggerState = Elm.Debugger.getHotSwapState();
            mainHandle.dispose();
            Elm.Debugger.dispose();

            var newMainNode = createMainElement();
            debuggerDiv.parentElement.insertBefore(newMainNode,debuggerDiv);

            var wrappedModule = Elm.debuggerAttach(module, debuggerState);
            mainHandle = Elm.embed(wrappedModule, newMainNode);
        }
        else {
            mainHandle = mainHandle.swap(module);
        }
    } else if (errorMessage) {
        var errorNode = document.createElement("pre");
        errorNode.id = "ErrorMessage";
        errorNode.innerHTML = errorMessage;
        errorNode.style.zindex = 1;
        errorNode.style.position = "absolute";
        errorNode.style.top = "0px";
        errorNode.style.background = ELM_LIGHT_GREY;

        var mainNode = document.getElementById(ELM_MAIN_ID);
        mainNode.parentElement.appendChild(errorNode);
    }
}
