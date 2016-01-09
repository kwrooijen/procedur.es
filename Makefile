LANGS:=$(shell ls config/languages -Q -m)

all:
	mkdir -p site/gen/js
	mkdir -p site/gen/json
	elm make src/Main.elm --output site/gen/js/procedures.js

build:
	mkdir -p site/gen/js	mkdir -p site/gen/json
	./scripts/build-json
	@echo '[$(LANGS)]' > site/gen/json/languages.json
