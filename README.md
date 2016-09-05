# kata-tennis

This repo contains an implementation of [Mark Seemann's KataTennis](https://github.com/ploeh/KataTennis) in Elm.

Mark wrote an article series called [Types + Properties = Software](http://blog.ploeh.dk/2016/02/10/types-properties-software/).

> The stronger a language's type system is, the more you can use static types to model your
> application's problem domain. With a sufficiently strong type system, you can make illegal states
> unrepresentable. While states are guaranteed to be legal, transitions between states need not be.
> You can often use Property-Based Testing to ensure that the state transitions are correct. The
> combination of types and properties give you a simple, but surprisingly powerful way of specifying
> the behaviour of the software you create.

Each article from the series has its own commit:

* [`ec3c836`](https://github.com/ravicious/kata-tennis/commit/ec3c836) – [Designing with types](http://blog.ploeh.dk/2016/02/10/types-properties-software/)
* [`8bb7123`](https://github.com/ravicious/kata-tennis/commit/8bb7123) – [State transition properties](http://blog.ploeh.dk/2016/02/11/types-properties-software-state-transition-properties)
* [`6e5b06f`](https://github.com/ravicious/kata-tennis/commit/6e5b06f) – [Properties for the advantage state](http://blog.ploeh.dk/2016/02/12/types-properties-software-properties-for-the-advantage-state)
* [`90fd83d`](https://github.com/ravicious/kata-tennis/commit/90fd83d) – [Properties for the Forties](http://blog.ploeh.dk/2016/02/15/types-properties-software-properties-for-the-forties)
* [`3b06ca9`](https://github.com/ravicious/kata-tennis/commit/3b06ca9) & [`3f61ad0`](https://github.com/ravicious/kata-tennis/commit/3f61ad0) & [`4b0fd64`](https://github.com/ravicious/kata-tennis/commit/4b0fd64) – [Composition](http://blog.ploeh.dk/2016/02/16/types-properties-software-composition)
* [`0ae1df3`](https://github.com/ravicious/kata-tennis/commit/0ae1df3) – [Initial state](http://blog.ploeh.dk/2016/02/17/types-properties-software-initial-state) & [Finite state machine](http://blog.ploeh.dk/2016/02/18/types-properties-software-finite-state-machine)
* [`22394a2`](https://github.com/ravicious/kata-tennis/commit/22394a2) – [Other properties](http://blog.ploeh.dk/2016/02/19/types-properties-software-other-properties)

## Running tests

To run tests, install [node-test-runner](https://github.com/rtfeldman/node-test-runner), go to the root of the repo and call `elm test` from the terminal.
