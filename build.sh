#!/usr/bin/env bash

./cleanup.sh

~/.cabal/bin/alex Lex.x
~/.cabal/bin/happy Synt.y
ghc LexMain.hs
ghc SyntMain.hs
ghc InterpretMain.hs
