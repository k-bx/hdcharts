hdcharts
===

Haskell-driven Charts

<img src="./test/combo.png" alt="alt text" width="400">

What is hdcharts?
===

This project is a way to create dynamic, updateable charts within a haskell environment.

Stack
---

* Chart Rendering - browser-based using the [d3js](http://d3js.org/) library
* Page rendering - [jmacro](https://hackage.haskell.org/package/jmacro), [lucid](https://hackage.haskell.org/package/lucid) and [clay](https://hackage.haskell.org/package/clay)
* Process - [websockets](https://hackage.haskell.org/package/websockets) and [mvc](https://hackage.haskell.org/package/mvc)
* Serving - [happstack](https://hackage.haskell.org/package/happstack-server)


How to run hdcharts
===

test/example.hs contains the working version of each current chart type.  Just:

1. `cabal build`
2. run `dist/build/example/example` (sandboxed of course!).
3. point browser to http://localhost:8001/
4. pick a chart and click Play button

Each chart has a testPlay function (that tests if it plays ok) which can provide a quick check and a starting point for developing your own.

Why does it exist?
===

The project is mostly a hobby-horse of mine.  I'm at the visual end of the analytical spectrum and static charts lead to dulled and static opinion.  Dynamic systems (which is pretty much everything) are best understood with the use of dynamic tools.


Why isn't the project using ghcjs, charts, diagrams, snap, gtk, tcp, ggplot ...
===

It might in the future.  The current stack is just where it's meandered to after a series of (potentially unfortunate) design decisions:

- any rendering outside of a browser is a little risky right now in terms of picking winners.  Even my nanna is watching tv inside a browser these days. And once you choose to target the browser, you choose javascript as the coding medium.  And within this context, d3js is the mature front-runner for browser charting excellence.

- Since I've never coded in javascript, I looked at ghcjs and retreated in fear.  It seemed a bit too monolithic to attempt something where lots of learning was needed.  And I couldn't see how I was going to hook up with the d3js library. Meanwhile, with jmacro I could start with d3 example snippets lying around, put quasi-quotes around them, and they would kind of work.

- chart looks fine but I couldn't work the dynamic angle out.  Maybe if I knew more about diagrams ...








