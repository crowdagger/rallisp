modules = \
  modules/dom/canvas.scm \
  modules/dom/document.scm \
  modules/dom/element.scm \
  modules/dom/event.scm \
  modules/dom/image.scm \
  modules/dom/media.scm \
  modules/dom/window.scm \
  modules/math.scm \
  modules/math/rect.scm \
  modules/math/vector.scm

game.wasm: game.scm $(modules)
	guild compile-wasm -L modules --bundle -o $@ $<

serve: game.wasm
	guile -c '((@ (hoot web-server) serve))'

bundle: game.wasm
	rm game.zip || true
	zip game.zip -r assets/ *.js *.css *.wasm index.html

clean:
	rm -f *.wasm reflect.js game.zip
