#!/usr/bin/env bash

./clean.sh

alex Lex.x
happy Synt.y
ghc LexMain.hs
ghc SyntMain.hs
