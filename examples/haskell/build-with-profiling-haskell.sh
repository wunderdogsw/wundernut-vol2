#!/bin/bash
ghc -package-db=.cabal-sandbox/x86_64-osx-ghc-7.10.1-packages.conf.d -Wall --make -O2 -prof -auto-all -caf-all -fforce-recomp src/Main.hs -o dist/MuhkeinSanapariHaskell
time dist/MuhkeinSanapariHaskell +RTS -p -RTS < alastalon_salissa.txt

