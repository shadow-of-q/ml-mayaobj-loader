all: native byte

native:
	ocamlbuild -no-hygiene -verbose 1 -libs str loadobj.native

byte:
	ocamlbuild -no-hygiene -verbose 1 -libs str loadobj.byte

