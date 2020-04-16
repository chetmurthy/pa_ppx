# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile

DESTDIR=
RM=rm

LAUNCH=env TOP=$(TOP) $(TOP)/tools/LAUNCH
OCAMLFIND=$(LAUNCH) ocamlfind

TARGET := pa_ppx_base.cma
CMO := ppxutil.cmo pa_passthru.cmo pa_unmatched_vala.cmo
CMI := $(CMO:.cmo=.cmi)

all: $(TARGET) $(TARGET:.cma=.cmxa) camlp5.pa_ppx_base local-install

camlp5.pa_ppx_base: $(TARGET)
	$(MKCAMLP5) -verbose -package camlp5.pa_r,camlp5.pr_r $(TARGET) -o $@

META: META.pl
	./META.pl > META
install::
	./META.pl $(DESTDIR)/lib > META
	$(OCAMLFIND) remove -destdir $(DESTDIR)/lib pa_ppx_base || true
	mkdir -p $(DESTDIR)/lib
	$(OCAMLFIND) install pa_ppx_base -destdir $(DESTDIR)/lib META $(TARGET) $(CMO:.cmo=.cmx) $(CMI)
	$(RM) -f META

local-install::
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

clean::
	rm -rf META camlp5.pa_ppx_base

$(TARGET): $(CMO)
	$(FINDLIB) ocamlc $(DEBUG) $(CMO) -a -o $(TARGET)

$(TARGET:.cma=.cmxa): $(CMO:.cmo=.cmx)
	$(OCAMLFIND) ocamlopt $(DEBUG) $(CMO:.cmo=.cmx) -a -o $(TARGET:.cma=.cmxa)

$(TARGET): $(CMO)
$(TARGET:.cma=.cmxa): $(CMO:.cmo=.cmx)

