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
PACKAGES_NOCAMLP5=rresult,fmt,pcre,ounit2
PACKAGES=$(PACKAGES_NOCAMLP5),camlp5

top: all

.ml.cmo:
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES) -syntax camlp5r -c $<

.SUFFIXES: .cmo .cmi .ml .mli

clean::
	$(RM) -f *.cm* *.byte *.log *.cache *.ppo *.ppo.ml

realclean:: clean
