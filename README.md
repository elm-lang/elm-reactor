# Elm Server

This is a simple server to make it easy to develop Elm projects.

View any kind of file in your browser. Elm files recompile every time you
refresh the page. The server also supports [hot-swapping][] and [time travel
debugging](http://debug.elm-lang.org) so you can have a great development
experience with any editor.

[hot-swapping]: http://elm-lang.org/blog/Interactive-Programming.elm

#### Install

It comes bundled with the Elm Platform or you can install it individually with
`cabal install elm-server`.


#### Use

Navigate to the Elm project you want to work on. In that directory run:

```bash
elm-server
```

This will start the server at [http://localhost:8000](http://localhost:8000).
If you want to use a different port, use the `--port` flag (e.g. `elm-server
--port 8080`). In your browser you can navigate through your project and see how
each Elm page looks.

`elm-server` will serve any kind of static content, so you can also look at HTML,
images, JSON, or whatever else you may need to serve.

#### Debug Mode

On the project navigation pages, all Elm files are paired with a little picture
of a wrench. Click that and you go into debug mode! Now you can:

  * Update the program by saving a new version in your editor. Updates propagate
    automatically!

  * Pause your program, rewind, and continue from any point.

  * Add watches and traces to track and visualize values over time.

#### Debug your own custom html file

If you like to play around with special libraries or have some custom css, the standard server setup may not work for you. You will want to insert the debugger where it is best for your file. It's easy:

First load the following script after you load the elm-runtime:
```html
<script type="text/javascript" src="/elm-runtime.js"></script>
<script type="text/javascript" src="/debugger.js"></script>
```
Then, initiate your elm program with the debugger:
```javascript
var runningElmModule = Elm.debugFullscreen(Elm.Todo, "todo.elm");
```
where `"todo.elm"` is the filepath of the main elm file relative to where you run the server.
