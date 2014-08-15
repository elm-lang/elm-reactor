// A note to the reader:
// This file is concatenated with debuggerInterface.elm's compiled
// javascript, toString.js, and debug-core.js. This is done at build time in Setup.hs

Elm.debugFullscreen = ElmRuntime.debugFullscreenWithOptions({
    socket: true,
    hotswapButton: true,
});
