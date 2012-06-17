#!/usr/local/bin/bash

rm Lex.{hs,hi,o}
rm Synt.{hs,hi,o}

alex Lex.x
happy Synt.y
ghc Lex.hs
ghc Synt.hs
