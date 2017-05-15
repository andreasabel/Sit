# Makefile for Sit

default :
	cabal install --only-dependencies
	cabal configure
	cabal build
	dist/build/Sit.bin/Sit.bin test/Test.agda

install :
	cabal install
