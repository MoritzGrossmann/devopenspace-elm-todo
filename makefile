.PHONY: build
build: elm
	$(MAKE) -C Backend/Haskell build

.PHONY: elm
elm:
	$(MAKE) -C Elm build

.PHONY: deploy
deploy:
	$(shell mkdir -p ./dist)
	$(shell mkdir -p ./dist/static)
	cp ./Backend/Haskell/static/* ./dist/static/ -r
	$(MAKE) -C Elm deploy
	$(MAKE) -C Backend/Haskell deploy
	cp ./Backend/Haskell/dist/* ./dist/ -r

.PHONY: clean
clean:
	$(MAKE) -C Elm clean
	$(MAKE) -C Backend/Haskell clean
	rm -rf ./dist

.PHONY: docker-build
docker-build:
	docker build -t todo-server -f ./Docker/app/dockerfile .

.PHONY: docker-run
docker-run: docker-build
	docker run -ti --rm -p 8080:8080 -v $(shell pwd)/data:/data todo-server
