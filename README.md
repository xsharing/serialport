![Haskell CI](https://github.com/standardsemiconductor/serialport/workflows/Haskell%20CI/badge.svg)
[![Hackage][hackage-badge]][hackage]
[![Hackage Dependencies][hackage-deps-badge]][hackage-deps]

Objectives
==========
* Cross platform: at least Linux, Windows and Mac OS.

Tests
=====

Setup
-----
* [Arduino Leonardo](http://arduino.cc/en/Main/arduinoBoardLeonardo) + [Sparkfun FTDI breakout board](https://www.sparkfun.com/products/718).
* Connections: TX, RX and GND

Prepare Arduino
---------------
* Upload arduino code using Arduino IDE or avrdude

Prepare haskell test program
----------------------------
* Configure cabal to build the tests: cabal configure --enable-tests.
* Build: cabal build

Running the tests
-----------------
* Run the tests: cabal test --test-options="/dev/ttyACM0 /dev/ttyUSB0"

[hackage]:            <https://hackage.haskell.org/package/serialport>
[hackage-badge]:      <https://img.shields.io/hackage/v/serialport.svg?color=success>
[hackage-deps-badge]: <https://img.shields.io/hackage-deps/v/serialport.svg>
[hackage-deps]:       <http://packdeps.haskellers.com/feed?needle=serialport>