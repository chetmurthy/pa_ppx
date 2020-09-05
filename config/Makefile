# Makefile,v
# Copyright (c) INRIA 2007-2017

RM=rm

OCAMLFIND=ocamlfind
OCAMLTOPLEVEL=ocaml
CAMLP5LIB=$(shell $(OCAMLFIND) query camlp5)
MKCAMLP5=mkcamlp5
CAMLP5R=camlp5r -I $(CAMLP5LIB)
INCLUDES=
OCAMLCFLAGS=$(DEBUG) $(WARNERR) $(INCLUDES)
PACKAGES=unix,compiler-libs.common,camlp5.quotations,camlp5.extfun
SYNTAX = camlp5r

export PERL5LIB := $(TOP):$(PERL5LIB)

top: all

.mli.cmi:
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) -package $(PACKAGES) -syntax $(SYNTAX) -c $<

.ml.cmo:
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) -package $(PACKAGES) -syntax $(SYNTAX) -c $<

.ml.cmx:
	$(OCAMLFIND) ocamlopt $(OCAMLCFLAGS) -package $(PACKAGES) -syntax $(SYNTAX) -c $<

.ml.cmt:
	@mkdir -p .cmt
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) -package $(PACKAGES),pa_ppx_dock -syntax $(SYNTAX) -bin-annot -c $< -o .cmt/$*.cmo
	cp .cmt/$*.cmt $*.cmt

.mli.cmti:
	@mkdir -p .cmt
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) -package $(PACKAGES),pa_ppx_dock -syntax $(SYNTAX) -bin-annot -c $< -o .cmt/$*.cmi
	cp .cmt/$*.cmti $*.cmti

.SUFFIXES: .cmo .cmi .ml .mli .cmx .cma .cmxa .cmt .cmti

clean::
	$(RM) -rf *.cm* *.o *.a *.byte *.log *.cache *.ppo *.ppo.ml _build/* .cmt *.odoc

realclean:: clean
