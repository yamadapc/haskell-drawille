drawille
========

A tiny library to write to the terminal using Braille characters, a port of
[Adam Tauber's (asciimoo) work on python drawille](https://github.com/asciimoo/drawille)
to haskell.

![Screenshot](screenshot.png)

The implementation still has some quirks, but it works (and is efficient).

## Installing

This package is available on hackage as
[`drawille`](http://hackage.haskell.org/package/drawille). You may install it
with cabal with:
```
cabal install drawille
```

## How to run tests

```
cabal configure --enable-tests && cabal build && cabal test
```

## License

This code is licensed under the GPL3 license. See [LICENSE](LICENSE) for more
information.
