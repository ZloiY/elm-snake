index.html: elm.js

elm.js: $(wildcard src/*.elm)
	elm make src/Snake.elm --output=elm.js

.PHONY=clean
clean:
	rm elm.js