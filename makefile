.PHONY: runHaskell
runHaskell: deployElm
	$(MAKE) -C Backend/Haskell run

.PHONY: deployElm
deployElm:
	$(MAKE) -C Elm deploy

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

.PHONY: docker-pack
docker-pack: docker-build
	docker save todo-server | gzip > todo-server.tar.gz
