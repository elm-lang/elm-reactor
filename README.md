# Elm Reactor

Interactive development tool that makes it easy to develop and debug Elm
programs. Key features include:

  * Automatically compile any Elm program
  * [Hot-swapping][]
  * [Time travel debugging][debug]
  * Compatible with any editor

[hot-swapping]: http://elm-lang.org/blog/Interactive-Programming.elm
[debug]: http://debug.elm-lang.org

This means you can get a great development experience whether you are using
Sublime Text, emacs, vim, or whatever else to edit Elm code.

## Install

Install [Elm Platform][platform]. This will install Elm Reactor and everything
else it needs.

[platform]: https://github.com/elm-lang/elm-platform

## Use

Navigate to the Elm project you want to work on. In that directory run:

```bash
elm-reactor
```

Now navigate to [http://localhost:8000](http://localhost:8000) which will show
a navigation page, letting you view any files in your project.

If you want to use a different port, use the `--port` flag (e.g. `elm-reactor
--port 8080`).

#### Time Travel Debugging

To use the debugger, click the small wrench next to every Elm file. This will
start your Elm program with a control panel that lets you:

  * Pause, rewind, and continue from any point.

  * Add watches and traces to track and visualize values over time.

  * Hot-swap in new code at any time, maintaining all recorded events. *See* what happens when you change your program.


#### Debugging code embedded in HTML

To use the debugger with more complex HTML or CSS, you may want to start the
debugger from within an HTML file.

In your custom HTML file load the `/debugger.js` script right after you load
elm-runtime:

```html
<script type="text/javascript" src="/elm-runtime.js"></script>
<script type="text/javascript" src="/debugger.js"></script>
```

That creates the `Elm.debugFullscreen` function so you can initiate your Elm
program with the debugger:

```javascript
var runningElmModule = Elm.debugFullscreen(Elm.Todo, "todo.elm");
```

The argument `"todo.elm"` is the file path to the root module of your project,
the one with a `main` value. This file path makes it possible to hot-swap when
you change any relevant code.