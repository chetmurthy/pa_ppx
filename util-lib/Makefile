# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile

DESTDIR=
RM=rm

LAUNCH=env TOP=$(TOP) $(TOP)/tools/LAUNCH
OCAMLFIND=$(LAUNCH) ocamlfind

PACKAGES := $(PACKAGES),pcre,core_kernel
TARGET := pa_ppx_utils.cma
CMO := std.cmo coll.cmo std2.cmo uf.cmo tsort0.cmo tsort.cmo
CMI := $(CMO:.cmo=.cmi)
CMX := $(CMO:.cmo=.cmx)

SYNTAX := camlp5o
OCAMLCFLAGS := $(OCAMLCFLAGS) -for-pack Pa_ppx_utils

all: $(TARGET) $(TARGET:.cma=.cmxa) local-install

pa_ppx_utils.cmo: $(CMO)
	$(OCAMLFIND) ocamlc $(DEBUG) -pack -o pa_ppx_utils.cmo $^

pa_ppx_utils.cmx: $(CMO:.cmo=.cmx)
	$(OCAMLFIND) ocamlopt $(DEBUG) -pack -o pa_ppx_utils.cmx $^

META: META.pl
	./META.pl > META
install::
	mkdir -p $(DESTDIR)/lib
	./META.pl $(DESTDIR)/lib > META
	$(OCAMLFIND)2 reinstall-if-diff pa_ppx_utils -destdir $(DESTDIR)/lib META $(TARGET) pa_ppx_utils.cmx pa_ppx_utils.cmi
	$(RM) -f META

local-install::
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

clean::
	rm -rf META

$(TARGET): pa_ppx_utils.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) $^ -a -o $(TARGET)

$(TARGET:.cma=.cmxa): pa_ppx_utils.cmx
	$(OCAMLFIND) ocamlopt $(DEBUG) $^ -a -o $(TARGET:.cma=.cmxa)

$(TARGET): $(CMO)
$(TARGET:.cma=.cmxa): $(CMO:.cmo=.cmx)

EXTERNAL := $(shell $(OCAMLFIND) query -predicates byte $(PACKAGES))
$(CMO) $(CMI) $(CMX): $(EXTERNAL)

depend::
	$(OCAMLFIND) ocamldep $(DEBUG) -package $(PACKAGES) -syntax $(SYNTAX) *.ml *.mli > .depend.NEW \
		&& mv .depend.NEW .depend

-include .depend
