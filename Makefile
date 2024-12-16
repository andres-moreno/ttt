all: ttt.wasm

ttt.wasm: ttt.scm
	guild compile-wasm -o ttt.wasm ttt.scm

serve: *.wasm
	guile -c '((@ (hoot web-server) serve))'

clean:
	rm *.wasm
