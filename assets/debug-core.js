// A note to the reader:
// This file is concatenated with debuggerInterface.elm's compiled
// javascript and toString.js. This is done at build time in Setup.hs

Elm.debugFullscreen = function(module, moduleFile, hotSwapState /* =undefined */) {

    var elmDebugger = {
        restart: function() {},
        pause: function() {},
        kontinue: function() {},
        getMaxSteps: function() { return 0; },
        stepTo: function(i) {},
        getPaused: function() { return false; },
        dispose: function() {},
        getHotSwapState: function() { return null; },
        watchTracker: { frames:[{}] }
    };

    var mainHandle = {};
    var debuggerHandle = {};
    var createdSocket = false;

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
        debugTools.style.right = "0px";
        debugTools.style.transitionDuration = "0.3s";
        debugTools.style.opacity = 0.97;
        debugTools.style.zIndex = 1;

        // Create and style the button
        var tabWidth = 25;
        var debugTab = document.createElement("div");
        debugTab.id = "debugToggle";
        debugTab.style.position = "absolute";
        debugTab.style.width = tabWidth + "px";
        debugTab.style.height = "60px";
        debugTab.style.top = window.innerHeight / 2 + "px";
        debugTab.style.left = "-" + tabWidth + "px";
        debugTab.style.borderTopLeftRadius = "3px";
        debugTab.style.borderBottomLeftRadius = "3px";
        debugTab.style.background = ELM_DARK_GREY;


        // Wire the button
        debugTab.onclick = function() {
            var toolPanel = document.getElementById("elmToolPanel");
            if (debuggingPanelExpanded){
                toolPanel.style.width = "0px";
                debuggingPanelExpanded = false;
            } else {
                toolPanel.style.right = "0px";
                toolPanel.style.width = debuggerWidth + "px";
                debuggingPanelExpanded = true;
            }
        }

        debugTools.appendChild(debugTab);
        debugTools.appendChild(debuggerDiv);
        return debugTools;
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
              watches: [],
              showHotswap: true
            });
        debuggerHandle.ports.scrubTo.subscribe(scrubber);
        debuggerHandle.ports.pause.subscribe(elmPauser);
        debuggerHandle.ports.restart.subscribe(elmRestart);
        debuggerHandle.ports.permitHotswap.subscribe(elmHotswap);
    }

    parent.window.addEventListener("message", function(e) {
        if (e.data === "elmDebuggerInit") {
            elmDebugger = Elm.Debugger;
            if (!createdSocket) {
                initSocket();
            }
        } else if (e.data === "elmNotify") {
            var currentPosition = elmDebugger.getMaxSteps();
            if (debuggerHandle.ports) {
                debuggerHandle.ports.eventCounter.send(currentPosition);
                sendWatches(currentPosition);
            }
        }
    }, false);


    function sendWatches(position) {
        var separator = "  ";
        var output = [];

        var watchAtPoint = elmDebugger.watchTracker.frames[position];

        for(key in watchAtPoint) {
            var value = watchAtPoint[key];
            // The toString object is defined in toString.js
            // and is prepended to this file at build time.
            var stringified = prettyPrint(value, separator);
            output.push([key, stringified]);
        }
        debuggerHandle.ports.watches.send(output);
    }

    function initSocket() {
        createdSocket = true;
        // "/todo.html" => "todo.elm"
        moduleFile = moduleFile || window.location.pathname.substr(1).split(".")[0] + ".elm";
        var socketLocation = "ws://" + window.location.host + "/socket?file=" + moduleFile;
        var serverConnection = new WebSocket(socketLocation);
        serverConnection.onmessage = function(event) {
            if (elmPermitHotswaps && debuggerHandle.ports) {
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
                debuggerDiv.parentElement.appendChild(newMainNode);

                var wrappedModule = Elm.debuggerAttach(module, debuggerState);
                mainHandle = Elm.embed(wrappedModule, newMainNode);

                // The div that rejects events must be after Elm
                var ignoringDiv = document.getElementById("elmEventIgnorer");
                if (ignoringDiv) {
                    ignoringDiv.parentNode.appendChild(ignoringDiv);
                }
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

    initDebugger();
    mainHandle = Elm.fullscreen(Elm.debuggerAttach(module, hotSwapState));
    return mainHandle;
}
