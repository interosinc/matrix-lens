help:
	@cat Makefile

build:
	stack build

develop:
	stack exec -- ghcid -c 'stack ghci' --restart stack.yaml --restart package.yaml

hlint:
	hlint src ./test

test:
	stack test

watch:
	stack test --fast --file-watch

b: build
d: develop
hl: hlint
t: test
w: watch
