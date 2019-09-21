.PHONY: build
build:
	npm run build

.PHONY: release
release:
	npm run release

deploy: build
	cp ./node_modules/todomvc-app-css/index.css ../dist/static/index.css
	cp ./node_modules/todomvc-common/base.css ../dist/static/base.css
	cp ./dist/ports.js ../dist/static/ports.js
	cp ./dist/app.js ../dist/static/todo.js

.PHONY: clean
clean: 
	rm -rf elm-stuff
	rm -rf dist