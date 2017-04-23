# puellascript

[![Build Status](https://travis-ci.org/TerrorJack/puellascript.svg)](https://travis-ci.org/TerrorJack/puellascript)

A proof-of-concept Haskell to WebAssembly compiler. The design goal is to implement:

* A framework to interface with GHC & Cabal and extract IRs
* A backend to translate STG/Cmm to WebAssembly binary code
* An rts written in WebAssembly to provide storage management, essential primops and FFI
* A packaging tool to group output into ECMAScript 6 modules and interact with foreign JavaScript toolchains like `npm`, `webpack`, etc

Currently we can extract multiple IRs from a set of target modules by invoking `ghc --make` and using hooks. It is not Cabal-aware yet. The next target is to retrieve STG/Cmm for wired-in packages, after which we can start delivering a working example.

This project works with GHC HEAD. [haddock docs](https://terrorjack.github.io/puellascript/haddock) is available (not always in sync though).
