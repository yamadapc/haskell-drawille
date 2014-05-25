drawille
========

A tiny library to write to the terminal using Braille characters, a port of
[Adam Tauber's (asciimoo) work on python drawille](https://github.com/asciimoo/drawille)
to haskell.

![Screenshot](screenshot.png)

The implementation still has some quirks, but it works (and is efficient).

## How to run tests

```
cabal configure --enable-tests && cabal build && cabal test
```

## License

This code is licensed under the GPL3 license. See [LICENSE](LICENSE) for more
information.
