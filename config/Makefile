# Makefile,v
# Copyright (c) INRIA 2007-2017

RM=rm

OCAMLFIND=ocamlfind
OCAMLTOPLEVEL=ocaml
CAMLP5LIB=$(shell $(OCAMLFIND) query camlp5)
MKCAMLP5=mkcamlp5
CAMLP5R=camlp5r -I $(CAMLP5LIB)
INCLUDES=-I $(CAMLP5LIB)
OCAMLCFLAGS=$(DEBUG) $(WARNERR) $(INCLUDES)
PACKAGES=camlp5

top: all

.mli.cmi:
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES) -syntax camlp5r -c $<

.ml.cmo:
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES) -syntax camlp5r -c $<

.ml.cmx:
	$(OCAMLFIND) ocamlopt $(DEBUG) -package $(PACKAGES) -syntax camlp5r -c $<

.SUFFIXES: .cmo .cmi .ml .mli .cmx .cma .cmxa

clean::
	$(RM) -f *.cm* *.o *.a *.byte *.log *.cache *.ppo *.ppo.ml _build/*

realclean:: clean
