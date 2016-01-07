LANGS:=$(shell ls config/languages -Q -m)

all:
	elm make src/Main.elm --output site/procedures.js

build:
	csvjson config/languages/guile/procs.csv > site/gen/json/guile-procs.json
	cp templates/Language.elm src/Gen/Language.elm
	sed -i s/{{LIST}}/'$(LANGS)'/ src/Gen/Language.elm
