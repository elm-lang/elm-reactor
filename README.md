# Merged into [`elm/compiler`](https://github.com/elm/compiler)


# Historical Note

In 2013, Laszlo Pandy figured out how to make a reliable [Time Traveling Debugger](http://debug.elm-lang.org/) for Elm. It became part of `elm-reactor` [in 2014](http://elm-lang.org/blog/time-travel-made-easy) thanks to Michael James who adapted it for how we *thought* people would work with it in practice. Since then (1) the internal implementation of Elm has changed a decent amount and (2) we have learned a lot about what production users need to use this tool seriously. So time travel is not available in `elm-reactor` right now, but it will be coming back with a new design focused on real workflows. I think this will lead us to a tool that is both more reliable and more broadly useful whether you are making games or web apps.

For now, the [`Debug`](https://package.elm-lang.org/packages/elm/core/latest/Debug) module still makes it easy to peek at values in your program.
