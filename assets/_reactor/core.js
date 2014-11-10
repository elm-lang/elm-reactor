// A note to the reader:
// This file is concatenated with debuggerInterface.elm's compiled
// javascript, toString.js, and reactor.js.
// This is done at build time in Setup.hs.

// Options:

// Expose internal swap function, disable swap button, no socket
// options.externalSwap = boolean

ElmRuntime.debugFullscreenWithOptions = function(options) {

    return function(module, moduleFile, swapState /* =undefined */) {
        var createdSocket = false;
        var elmPermitSwaps = true;

        var ELM_DEBUGGER_ID = "elmToolPanel";
        var ELM_DARK_GREY = "#4A4A4A";
        var ELM_LIGHT_GREY = "#E4E4E4";

        var mainHandle = Elm.fullscreenDebugHooks(module, swapState);
        var debuggerHandle = initDebugger();
        if (!options.externalSwap) {
            initSocket();
        }

        parent.window.addEventListener("message", function(e) {
            if (e.data === "elmNotify") {
                var currentPosition = mainHandle.debugger.getMaxSteps();
                if (debuggerHandle.ports) {
                    debuggerHandle.ports.eventCounter.send(currentPosition);
                    sendWatches(currentPosition);
                }
            }
        }, false);

        function createDebuggingElement() {
            var debuggingPanelExpanded = true;
            var debuggerWidth = 275;

            var debugTools = document.createElement("div");
            debugTools.id = ELM_DEBUGGER_ID;

            var debuggerDiv = document.createElement("div");
            debuggerDiv.id = "elmDebugger";
            debuggerDiv.style.overflow = "hidden";
            debuggerDiv.style.overflowY = "auto";
            debuggerDiv.style.height = "100%";

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

            // Prevent clicks from reaching the main elm instance under the panel
            function stopEvents(e) {
                if (!e) {
                    var e = window.event;
                }
                e.cancelBubble = true;
                if (e.stopPropagation) {
                    e.stopPropagation();
                }
            }
            debugTools.addEventListener("click", stopEvents);

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
            };

            debugTools.appendChild(debugTab);
            debugTools.appendChild(debuggerDiv);
            return debugTools;
        }

        function initDebugger() {
            function scrubber(position) {
                if (mainHandle.debugger.getPaused()) {
                    mainHandle.debugger.stepTo(position);
                    sendWatches(position);
                }
            }

            function elmPauser(doPause) {
                if (doPause) {
                  mainHandle.debugger.pause();
                } else {
                    mainHandle.debugger.kontinue();
                }
            }

            function elmRestart() {
                mainHandle.debugger.restart();
                sendWatches(0);
            }

            function elmSwap(permitSwaps) {
                elmPermitSwaps = permitSwaps;
            }

            var debugTools = createDebuggingElement();
            document.body.appendChild(debugTools);
            var debuggerDiv = document.getElementById("elmDebugger");

            var handle = Elm.embed(Elm.DebuggerInterface, debuggerDiv,
                { eventCounter: 0,
                  watches: [],
                  showSwap: !options.externalSwap
                });
            handle.ports.scrubTo.subscribe(scrubber);
            handle.ports.pause.subscribe(elmPauser);
            handle.ports.restart.subscribe(elmRestart);
            handle.ports.permitSwap.subscribe(elmSwap);
            return handle;
        }

        function sendWatches(position) {
            var separator = "  ";
            var output = [];

            var watchAtPoint = mainHandle.debugger.watchTracker.frames[position];

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
                if (elmPermitSwaps && debuggerHandle.ports) {
                    swap(event.data);
                }
            };
            window.addEventListener("unload", function() {
                serverConnection.close();
            });
        }

        function swap(raw) {
            var debuggerDiv = document.getElementById(ELM_DEBUGGER_ID);
            var result = JSON.parse(raw);
            var js = result.success;
            var errorMessage = result.error;
            var error = document.getElementById('ErrorMessage');
            if (error) {
                error.parentNode.removeChild(error);
            }
            if (js) {
                window.eval(js);
                var moduleStr = js.match(/(Elm\..+)\ =\ \1/)[1];
                var module = window.eval(moduleStr);
                if (mainHandle.debugger) {
                    var debuggerState = mainHandle.debugger.getSwapState();
                    mainHandle.debugger.dispose();
                    mainHandle.dispose();

                    mainHandle = Elm.fullscreenDebugHooks(module, debuggerState);

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
                errorNode.style.top = "0";
                errorNode.style.left = "0";
                errorNode.style.color = ELM_DARK_GREY;
                errorNode.style.backgroundColor = ELM_LIGHT_GREY;
                errorNode.style.padding = "1em";
                errorNode.style.margin = "1em";
                errorNode.style.borderRadius = "10px";

                document.body.appendChild(errorNode);
            }
        }

        if (options.externalSwap) {
            mainHandle.debugger.swap = swap;
        }
        return mainHandle;
    };
};
