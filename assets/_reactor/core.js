// A note to the reader:
// This file is concatenated with debuggerInterface.elm's compiled
// javascript, toString.js, and reactor.js.
// This is done at build time in Setup.hs.

// Options:

// Expose internal swap function, disable swap button, no socket
// options.externalSwap = boolean

function debugFullscreenWithOptions(options) {

    return function(module, moduleFile, swapState /* =undefined */) {
        var createdSocket = false;
        var elmPermitSwaps = true;

        var ELM_DEBUGGER_ID = "elmToolPanel";
        var ELM_DARK_GREY = "#4A4A4A";
        var ELM_LIGHT_GREY = "#E4E4E4";

        var mainHandle = Elm.fullscreenDebugHooks(module, swapState);
        var debuggerHandles = initDebugger();
        var watchesHandle = debuggerHandles[0];
        var sliderHandle = debuggerHandles[1];
        if (!options.externalSwap) {
            initSocket();
        }

        parent.window.addEventListener("message", function(e) {
            if (e.data === "elmNotify") {
                var currentPosition = mainHandle.debugger.getMaxSteps();
                if (sliderHandle.ports && watchesHandle.ports) {
                    sliderHandle.ports.eventCounter.send(currentPosition);
                    sendWatches(currentPosition);
                }
            }
        }, false);

        function createDebuggingElement() {
            var debuggingPanelExpanded = true;
            var debuggerWidth = 275;

            var debugTools = document.createElement("div");
            debugTools.id = ELM_DEBUGGER_ID;

            // Create space for watches that scrolls
            var watchesDiv = document.createElement("div");
            watchesDiv.id = "elmWatches";
            watchesDiv.style.overflowY = "auto";
            watchesDiv.style.overflowX = "hidden";
            watchesDiv.style.height = "calc(100% - 150px)";

            // Create space for slider
            var sliderDiv = document.createElement("div");
            sliderDiv.id = "elmSlider";

            // Group slider and watches
            var debuggerDiv = document.createElement("div");
            debuggerDiv.id = "elmDebugger";
            debuggerDiv.style.overflow = "hidden";
            debuggerDiv.style.height = "100%";

            debuggerDiv.appendChild(sliderDiv);
            debuggerDiv.appendChild(watchesDiv);

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
            var sliderDiv = document.getElementById("elmSlider");

            // Wire up watches
            var elmWatches = document.getElementById("elmWatches");
            var watchesHandle = Elm.embed(Elm.Watches, elmWatches,
                { watches: [] });

            // Wire up Slider
            var slideHandle = Elm.embed(Elm.DebuggerInterface, sliderDiv,
                { eventCounter: 0,
                  showSwap: !options.externalSwap
                });
            slideHandle.ports.scrubTo.subscribe(scrubber);
            slideHandle.ports.pause.subscribe(elmPauser);
            slideHandle.ports.restart.subscribe(elmRestart);
            slideHandle.ports.permitSwap.subscribe(elmSwap);

            return [watchesHandle, slideHandle];
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
            watchesHandle.ports.watches.send(output);
        }

        function initSocket() {
            createdSocket = true;
            // "/todo.html" => "todo.elm"
            moduleFile = moduleFile || window.location.pathname.substr(1).split(".")[0] + ".elm";
            var socketLocation = "ws://" + window.location.host + "/socket?file=" + moduleFile;
            var serverConnection = new WebSocket(socketLocation);
            serverConnection.onmessage = function(event) {
                if (elmPermitSwaps && sliderHandle.ports && watchesHandle.ports) {
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
