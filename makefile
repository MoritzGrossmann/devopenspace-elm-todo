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
	docker-compose -f ./Docker/docker-compose.yml --project-directory . -p todo build --force-rm

.PHONY: docker-run
docker-run:
	docker-compose -f ./Docker/docker-compose.yml --project-directory . -p todo up

.PHONY: docker-install
docker-install:
	docker-compose -f ./Docker/docker-compose.yml --project-directory . -p todo up -d

.PHONY: docker-clean
docker-clean:
	docker-compose -f ./Docker/docker-compose.yml --project-directory . -p todo rm -s -f -v