.PHONY: build
build:
	stack build

.PHONY: run
run: build
	stack exec TodoServer-exe

.PHONY: clean
clean: 
	rm -rf .stack-work
	rm -rf ./dist

deploy: build
	mkdir -p ./dist
	cp $(shell stack path --local-install-root)/bin/. ./dist/ -a