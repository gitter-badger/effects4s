# Effects4s

[![Build Status](https://travis-ci.org/monix/schrodinger.svg?branch=master)](https://travis-ci.org/monix/schrodinger)

Common communication protocol for `IO` / `Task` data types.

Aims to be a set of common interfaces to abstract over `cats.Eval`,
`monix.eval.Task`, `fs2.Task`, `scalaz.concurrent.Task`, `scalaz.effects.IO`,
`scala.concurrent.Future` or other data-types that evaluate side-effects and
trigger single results, lazily or asynchronously.

Or in other words, this aims to be the 
**[Reactive Streams](http://www.reactive-streams.org/)**
protocol of `IO`, `Task` and `Future` data types.

