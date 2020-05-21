help:
	@cat Makefile

build:
	stack build

cabal-build-in-docker:
	docker build .

cabal-check:
	stack exec -- cabal check

develop:
	stack exec -- ghcid -c 'stack ghci' --restart stack.yaml --restart package.yaml

hlint:
	hlint src ./test

test:
	stack test

watch:
	stack test --fast --file-watch

b: build
cb: cabal-build-in-docker
cc: cabal-check
d: develop
hl: hlint
t: test
w: watch
