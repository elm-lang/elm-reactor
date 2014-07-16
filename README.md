# Elm Server

This is a simple server to make it easier to develop Elm projects. It lets you
view Elm files in your browser and recompile them by refreshing the page. The
server also support hotswapping on file changes (see the [blog post]
(http://elm-lang.org/blog/Interactive-Programming.elm) about what hotswapping is).

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

To begin navigate to a directory of your choice and click the wrench.
You will be brought to the debugger. On the bottom are time-travelling
controls and above is your elm program. When you make a change to the
source file and save it, the website will update accordingly.

You may hot swap code at any point and maintain program state as long
as the type signatures don't change.

#### Program assets

Your elm program may use local files during development. It is recommended
that you place them in a directory called `assets` on the root of the project.
In your elm code would look something like:

```haskell
      let marioImage = image 35 35 "/imgs/mario-stading.gif"
```
And the corresponding directory structure should be:
```
<project root>
    assets/
        imgs/
            mario-standing.gif
```
