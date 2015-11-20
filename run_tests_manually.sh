#!/bin/bash
PATH=$PATH:.cabal-sandbox/bin
runhaskell -i:src:test test/Spec.hs $1
