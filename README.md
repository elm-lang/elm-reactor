# Elm Reactor

Interactive development tool that makes it easy to develop and debug Elm
programs. Key features include:

  * Automatically compile any Elm program
  * [Hot-swapping][]
  * [Time travel debugging][debug]
  * Compatible with any editor

[hot-swapping]: http://elm-lang.org/blog/interactive-programming
[debug]: http://elm-lang.org/blog/time-travel-made-easy

This means you can get a great development experience whether you are using
Sublime Text, Emacs, vim, or whatever else to edit Elm code.


## Install

Install [Elm Platform][platform]. This will install Elm Reactor and everything
else it needs.

[platform]: http://elm-lang.org/install


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


## Building from Source

If you are interesting in modifying this project, here is what you need to do
as of 20 November 2015:

### Build Elm Platform

```bash
mkdir sandbox
cd sandbox
cabal sandbox init

git clone https://github.com/elm-lang/elm-compiler.git
cd elm-compiler
cabal sandbox init --sandbox ../.cabal-sandbox
cd ..

git clone https://github.com/elm-lang/elm-package.git
cd elm-package
cabal sandbox init --sandbox ../.cabal-sandbox
cd ..

git clone https://github.com/elm-lang/elm-make.git
cd elm-make
cabal sandbox init --sandbox ../.cabal-sandbox
cd ..

git clone https://github.com/elm-lang/core.git
cd core
git remote add vilterp git@github.com:vilterp/core.git
git fetch vilterp new-debugger-api-support
git checkout new-debugger-api-support
cd ..

git clone https://github.com/elm-lang/elm-reactor.git
cd elm-reactor
cabal sandbox init --sandbox ../.cabal-sandbox
git checkout expando
elm-package install
cd elm-stuff/packages/elm-lang/core
rm -rf 3.0.0
ln -s ../../../../../core 3.0.0
cd ../../../..

cd ../elm-compiler
cabal install
cd ../elm-package
cabal install
cd ../elm-make
cabal install
cd ../elm-reactor
git submodule init
git submodule update
```

Then most of your work will be in `elm-reactor/`, where you recompile with this:

```
./cabal-build.py
```

This does an extra check to see if any Elm files have changed and recompiles
accordingly. You will need to run this for any changes you make, whether they
are in Haskell or Elm.

The binaries will all go in `sandbox/.cabal-sandbox/bin` so that is where you
want to point for testing.

### Run Development Reactor

cd into the directory of the project you want to debug, then:

```
elm-make --yes
pushd elm-stuff/packages/elm-lang/core
rm -rf 3.0.0
ln -s <path to core on branch new-debugger-api-support from vilterp fork of core> 3.0.0
popd
```

...then invoke `elm-reactor` as normal and you should be able to use the new reactor. This linking step will be necessary until a version of core is published which has the changes on `vilterp-new-debugger-api-support` integrated.
