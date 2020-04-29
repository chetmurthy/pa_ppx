# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile

DESTDIR=
RM=rm

LAUNCH=env TOP=$(TOP) $(TOP)/tools/LAUNCH
OCAMLFIND=$(LAUNCH) ocamlfind

PACKAGES := $(PACKAGES),rresult,fmt
TARGET := pa_ppx_base.cma
CMO := ppxutil.cmo pa_passthru.cmo
CMI := $(CMO:.cmo=.cmi)
OCAMLCFLAGS := $(OCAMLCFLAGS) -for-pack Pa_ppx_base

all: $(TARGET) $(TARGET:.cma=.cmxa) camlp5.pa_ppx_base local-install

pa_ppx_base.cmo: $(CMO)
	$(OCAMLFIND) ocamlc $(DEBUG) -pack -o pa_ppx_base.cmo $^

pa_ppx_base.cmx: $(CMO:.cmo=.cmx)
	$(OCAMLFIND) ocamlopt $(DEBUG) -pack -o pa_ppx_base.cmx $^

camlp5.pa_ppx_base: $(TARGET)
	$(MKCAMLP5) -verbose -package fmt,rresult,camlp5.pa_r,camlp5.pr_r $(TARGET) -o $@

META: META.pl
	./META.pl > META
install::
	./META.pl $(DESTDIR)/lib > META
	$(OCAMLFIND) remove -destdir $(DESTDIR)/lib pa_ppx_base || true
	mkdir -p $(DESTDIR)/lib
	$(OCAMLFIND) install pa_ppx_base -destdir $(DESTDIR)/lib META $(TARGET) pa_ppx_base.cmx pa_ppx_base.cmi
	$(RM) -f META

local-install::
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

clean::
	rm -rf META camlp5.pa_ppx_base

$(TARGET): pa_ppx_base.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) $^ -a -o $(TARGET)

$(TARGET:.cma=.cmxa): pa_ppx_base.cmx
	$(OCAMLFIND) ocamlopt $(DEBUG) $^ -a -o $(TARGET:.cma=.cmxa)

$(TARGET): $(CMO)
$(TARGET:.cma=.cmxa): $(CMO:.cmo=.cmx)

depend::
	$(OCAMLFIND) ocamldep $(DEBUG) -package $(PACKAGES) -syntax camlp5r *.ml *.mli > .depend.NEW \
		&& mv .depend.NEW .depend

-include .depend
