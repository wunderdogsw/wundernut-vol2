#!/bin/bash
ghc -package-db=.cabal-sandbox/x86_64-osx-ghc-7.10.1-packages.conf.d -Wall --make -O2 -fforce-recomp src/Main.hs -o dist/Main
time dist/Main < alastalon_salissa.txt

