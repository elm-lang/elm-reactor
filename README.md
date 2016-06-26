# Elm Reactor

Interactive development tool that makes it easier to develop Elm
programs. Key features include:

  * Automatically compile any Elm program
  * Compatible with any editor

This means you can get a great development experience whether you are using
Sublime Text, Emacs, vim, or whatever else to edit Elm code.


## Install

Install [Elm Platform][platform]. This will install Elm Reactor and everything
else it needs.

[platform]: http://elm-lang.org/install


## Use

Navigate to the Elm project you want to work on. A great project to get started
with is [elm-architecture-tutorial][] which contains some simple programs that should be fun
to debug.

[elm-architecture-tutorial]: https://github.com/evancz/elm-architecture-tutorial

In the root of your Elm project start the reactor with:

```bash
elm-reactor
```

Now open [http://localhost:8000](http://localhost:8000) in your browser. You
should see a navigation page for your project.

Click on any file to see what it looks like. For example, you can navigate to an Elm file like `examples/1-button.elm` and try it out. If you modify the file, you can just refresh that page and see the new version!

### Note on Elm's Time Traveling Debugger
[Elm's Time Traveling Debugger](http://debug.elm-lang.org/) has been temporarily disabled.
It is not currently available as part of the `elm-reactor`.
An updated version will appear in a future release.
For now, please use the [Debug](http://package.elm-lang.org/packages/elm-lang/core/latest/Debug) library.
