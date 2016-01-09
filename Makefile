LANGS:=$(shell ls config/languages -Q -m)

all:
	elm make src/Main.elm --output site/procedures.js

build:
	./scripts/build-json
	@echo '[$(LANGS)]' > site/gen/json/languages.json
