.PHONY: all init

package = $(or $(filter-out skeleton,$(patsubst %.cabal,%,$(wildcard *.cabal))),$(notdir $(realpath .)))

all: dist/setup-config $(package).cabal
	cabal build
	cabal test

$(package).cabal:
	sed -i -e 's/_PACKAGE_/$(package)/g' \
		.gitignore \
		LICENSE \
		README.md \
		Setup.lhs \
		program/Main.hs \
		skeleton.cabal \
		test/Bench.hs \
		test/DocTest.hs \
		test/Props.hs \
		test/Test.hs
	mv skeleton.cabal $(package).cabal
	ln -s dist/build/bench/bench
	ln -s dist/build/doctest/doctest
	ln -s dist/build/$(package)/$(package)
	ln -s dist/build/$(package)-test/$(package)-test
	ln -s dist/build/tests/tests
	git init
	git remote add origin git@github.com:ertes/$(package).git
	git add \
		LICENSE Makefile README.md Setup.lhs .gitignore \
		$(package).cabal program test

dist/setup-config: $(package).cabal
	cabal configure --enable-benchmarks --enable-tests
