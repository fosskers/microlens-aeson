This project was started by Paul Wilson
[@statusfailed](https://github.com/statusfailed).

Edward Kmett stole it and decided he was going to polish it up and put it on
Hackage. In the process he took all sorts of liberties with the structure of
the project. If you don't like the result, it is probably his fault.

* [Edward Kmett](mailto:ekmett@gmail.com) [@ekmett](https://github.com/ekmett)

In late 2015, Colin Woodbury [@fosskers](https://github.com/fosskers)
further stole this library and converted it to use `microlens`. This was to
be used in a potential fork of
[wreq](http://hackage.haskell.org/package/wreq), which uses the full `lens`
library and all its dependencies.

This change ripped out all `Prism`s in favour of `Traversal`s, which are
sufficient for basic manipulations of Aeson data.
