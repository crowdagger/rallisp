modules = \
  modules/dom/*.scm \
  modules/*.scm \
  modules/rallisp/*.scm \
  modules/levels/*.scm \
  modules/math/*.scm 

game.wasm: game.scm $(modules)
	guild compile-wasm -L modules --bundle -o $@ $<

serve: game.wasm
	guile -c '((@ (hoot web-server) serve))'

bundle: game.wasm
	rm game.zip || true
	zip game.zip -r assets/ *.js *.css *.wasm index.html

clean:
	rm -f *.wasm reflect.js game.zip
