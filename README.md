# shush [![Build Status](https://img.shields.io/travis/irreverent-pixel-feats/shush.svg?style=flat)](https://travis-ci.org/irreverent-pixel-feats/shush)

"shell out" library that wraps around process

This is for actually shelling out (which is pretty "savage").

And it tries to follow the same `withBinaryFile ...` like pattern you might use when you read files.

Other options that take the "we'll provide actual functions that are named similar to unix shell commands
and do similar things without shelling out":

- [Shelly](https://github.com/yesodweb/Shelly.hs)
- [Turtle](https://github.com/Gabriel439/Haskell-Turtle-Library)

Those are probably more worthwhile looking into, but this library is for when there are functions that
are unavailable in the above libraries or you are writing a haskell shim to manage a multiprocess
application.

## Building the project

```
./mafia build
```

## Running Unit Tests

```
./mafia test
```
