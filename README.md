# Elm Server

This is a simple server to make it easier to develop Elm projects. It lets you
view Elm files in your browser and recompile them by refreshing the page.

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
