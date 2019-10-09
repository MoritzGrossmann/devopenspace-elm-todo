npmInstall: package.json
	npm install

.PHONY: build
build: npmInstall
	npm run build

deploy: npmInstall
	npm run bundleJS
	cp ./node_modules/todomvc-app-css/index.css ../Backend/Haskell/static/index.css
	cp ./node_modules/todomvc-common/base.css ../Backend/Haskell/static/base.css
	cp ./static/app.css ../Backend/Haskell/static/app.css
	cp ./dist/todo.js ../Backend/Haskell/static/todo.js

.PHONY: clean
clean:
	rm -rf elm-stuff
	rm -rf dist
