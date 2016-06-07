## Build from Source

If you are interesting in modifying this project, here is what you need to do
as of 20 November 2015:

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