# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile

DESTDIR=
RM=rm

LAUNCH=$(TOP)/tools/LAUNCH
OCAMLFIND=$(LAUNCH) ocamlfind
MKCAMLP5=$(LAUNCH) mkcamlp5
PACKAGES := $(PACKAGES),pa_ppx_base,fmt,bos,compiler-libs.common

TARGET=pa_undo_deriving.cma
CMO=pa_undo_deriving.cmo
CMI := $(CMO:.cmo=.cmi)
CMX := $(CMO:.cmo=.cmx)

all: $(TARGET) $(TARGET:.cma=.cmxa) camlp5.pa_ppx_undo_deriving local-install

camlp5.pa_ppx_undo_deriving:
	$(MKCAMLP5) -verbose -package $(PACKAGES) $(TARGET) -o $@

META: META.pl
	./META.pl > META

install::
	./META.pl $(DESTDIR)/lib > META
	mkdir -p $(DESTDIR)/lib
	$(OCAMLFIND)2 reinstall-if-diff pa_ppx_undo_deriving -destdir $(DESTDIR)/lib META $(TARGET) $(TARGET:.cma=.cmxa) $(CMI)
	$(RM) -f META

local-install::
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

clean::
	rm -rf META camlp5.pa_ppx_undo_deriving

$(TARGET): $(CMO)
	$(OCAMLFIND) ocamlc $(DEBUG) $(CMO) -a -o $(TARGET)

$(TARGET:.cma=.cmxa): $(CMO:.cmo=.cmx)
	$(OCAMLFIND) ocamlopt $(DEBUG) $(CMO:.cmo=.cmx) -a -o $(TARGET:.cma=.cmxa)

$(TARGET): $(CMO)
$(TARGET:.cma=.cmxa): $(CMO:.cmo=.cmx)

EXTERNAL := $(shell $(OCAMLFIND) query -predicates byte $(PACKAGES))
$(CMO) $(CMI) $(CMX): $(EXTERNAL)

depend::
	$(OCAMLFIND) ocamldep $(DEBUG) -package $(PACKAGES) -syntax camlp5r *.ml *.mli > .depend.NEW \
		&& mv .depend.NEW .depend

-include .depend
