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

[platform]: http://elm-lang.org/Install.elm

## Use

Navigate to the Elm project you want to work on. A great project to get started
with is [elm-examples][] which contains some simple programs that should be fun
to debug.

[elm-examples]: https://github.com/evancz/elm-examples

In the root of your Elm project start the reactor with:

```bash
elm-reactor
```

Now open [http://localhost:8000](http://localhost:8000) in your browser. You
should see a navigation page for your project.

Click on any file to see what it looks like. For example, you can navigate to
an Elm file and try it out. If you modify the file, you can just refresh that
page and see the new version!

#### Time Travel Debugging

To use the debugger, click the small wrench next to every Elm file. This will
start your Elm program with a control panel that lets you:

  * Pause, rewind, and continue from any point.

  * Add watches and traces to track and visualize values over time.

  * Swap in new code at any time, maintaining all recorded events.


#### Debugging code embedded in HTML

To use the debugger with more complex HTML or CSS, you may want to start the
debugger from within an HTML file. This process is still improving, so *use this
with caution*.

In your custom HTML file, load the `/_reactor/debug.js` script.

```html
<script type="text/javascript" src="/_reactor/debug.js"></script>
```

That creates the `Elm.fullscreenDebug` function so you can initiate your Elm
program with the debugger:

```javascript
var main = Elm.fullscreenDebug('MyProject.Main', 'MyProject/Main.elm');
```

The first argument is the name of the module you would like to debug.
The second argument is the name of the source file for that module.
The file name is needed so that we know which file to recompile when
the Reactor detects that a file has changed. You may edit a dependency,
but we always need to compile from the root file.
