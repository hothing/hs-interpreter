#!/usr/local/bin/bash

rm Lex.{hs,hi,o}

alex Lex.x
ghc Lex.hs
