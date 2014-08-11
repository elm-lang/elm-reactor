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
    var tabWidth = 40;
    var debugTab = document.createElement("div");
    debugTab.id = "debugToggle";
    debugTab.style.position = "absolute";
    debugTab.style.width = tabWidth + "px";
    debugTab.style.height = "40px";
    debugTab.style.top = window.innerHeight / 2 + "px";
    debugTab.style.left = "-" + tabWidth + "px";
    debugTab.style.borderTopLeftRadius = "3px";
    debugTab.style.borderBottomLeftRadius = "3px";
    debugTab.style.background = ELM_DARK_GREY;

    var wrenchIcon = document.createElement("img");
    wrenchIcon.src = "/debug-wrench-elm-server.png";
    wrenchIcon.height = 30;
    wrenchIcon.title = "Debug panel";
    wrenchIcon.style.margin = "5px";
    debugTab.appendChild(wrenchIcon);

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
          watches: []
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
    var separator = "  ";
    var output = [];

    var watchAtPoint = elmDebugger.watchTracker.frames[position];

    for(key in watchAtPoint) {
        var value = watchAtPoint[key];
        var stringified = toString(value, separator);
        output.push([key, stringified]);
    }
    
    debuggerHandle.ports.watches.send(output);
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








// Utilities

var toString = function(v, separator) {
    var type = typeof v;
    if (type === "function") {
        var name = v.func ? v.func.name : v.name;
        return '<function' + (name === '' ? '' : ': ') + name + '>';
    } else if (type === "boolean") {
        return v ? "True" : "False";
    } else if (type === "number") {
        return v.toFixed(2).replace(/\.0+$/g, '');
    } else if ((v instanceof String) && v.isChar) {
        return "'" + addSlashes(v) + "'";
    } else if (type === "string") {
        return '"' + addSlashes(v) + '"';
    } else if (type === "object" && '_' in v && probablyPublic(v)) {
        var output = [];
        for (var k in v._) {
            for (var i = v._[k].length; i--; ) {
                output.push(k + " = " + toString(v._[k][i], separator));
            }
        }
        for (var k in v) {
            if (k === '_') continue;
            output.push(k + " = " + toString(v[k], separator));
        }
        if (output.length === 0) return "{}";
        var body = "\n" + output.join(",\n");
        return "{" + body.replace(/\n/g,"\n" + separator) + "\n}";
    } else if (type === "object" && 'ctor' in v) {
        if (v.ctor.substring(0,6) === "_Tuple") {
            var output = [];
            for (var k in v) {
                if (k === 'ctor') continue;
                output.push(toString(v[k], separator));
            }
            return "(" + output.join(", ") + ")";
        } else if (v.ctor === "_Array") {
            var list = Array.toList(v);
            return "Array.fromList " + toString(list, separator);
        } else if (v.ctor === "::") {
            var output = '[\n' + toString(v._0, separator);
            v = v._1;
            while (v.ctor === "::") {
                output += ",\n" + toString(v._0, separator);
                v = v._1;
            }
            output += ' ]';
            return output.replace(/\n/g,"\n" + separator);
        } else if (v.ctor === "[]") {
            return "[]";
        } else if (v.ctor === "RBNode" || v.ctor === "RBEmpty") {
            var cons = F3(function(k,v,acc){return NList.Cons(Tuple2(k,v),acc)});
            var list = A3(Dict.foldr, cons, NList.Nil, v);
            var name = "Dict";
            if (list.ctor === "::" && list._0._1.ctor === "_Tuple0") {
                name = "Set";
                list = A2(List.map, function(x){return x._0}, list);
            }
            return name + ".fromList " + toString(list, separator);
        } else {
            var output = "";
            for (var i in v) {
                if (i === 'ctor') continue;
                var str = toString(v[i], separator);
                var parenless = str[0] === '{' ||
                                str[0] === '<' ||
                                str[0] === "[" ||
                                str.indexOf(' ') < 0;
                output += ' ' + (parenless ? str : "(" + str + ')');
            }
            return v.ctor + output;
        }
    }
    if (type === 'object' && 'recv' in v) return '<signal>';
    return "<internal structure>";
};

function addSlashes(str) {
    return str.replace(/\\/g, '\\\\')
              .replace(/\n/g, '\\n')
              .replace(/\t/g, '\\t')
              .replace(/\r/g, '\\r')
              .replace(/\v/g, '\\v')
              .replace(/\0/g, '\\0')
              .replace(/\'/g, "\\'")
              .replace(/\"/g, '\\"');
}

function probablyPublic(v) {
    var keys = Object.keys(v);
    var len = keys.length;
    if (len === 3
        && 'props' in v
        && 'element' in v) return false;
    if (len === 5
        && 'horizontal' in v
        && 'vertical' in v
        && 'x' in v
        && 'y' in v) return false;
    if (len === 7
        && 'theta' in v
        && 'scale' in v
        && 'x' in v
        && 'y' in v
        && 'alpha' in v
        && 'form' in v) return false;
    return true;
}
