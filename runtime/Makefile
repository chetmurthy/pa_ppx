# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile

DESTDIR=
RM=rm

LAUNCH=env TOP=$(TOP) $(TOP)/tools/LAUNCH
OCAMLFIND=$(LAUNCH) ocamlfind

PACKAGES := rresult,fmt,camlp5,yojson
TARGET := pa_ppx_runtime.cma
CMO := runtime.cmo
CMI := $(CMO:.cmo=.cmi)
OCAMLCFLAGS := $(OCAMLCFLAGS) -for-pack Pa_ppx_runtime

all: $(TARGET) $(TARGET:.cma=.cmxa) local-install

pa_ppx_runtime.cmo: $(CMO)
	$(OCAMLFIND) ocamlc $(DEBUG) -pack -o pa_ppx_runtime.cmo $^

pa_ppx_runtime.cmx: $(CMO:.cmo=.cmx)
	$(OCAMLFIND) ocamlopt $(DEBUG) -pack -o pa_ppx_runtime.cmx $^

META: META.pl
	./META.pl > META
install::
	./META.pl $(DESTDIR)/lib > META
	$(OCAMLFIND) remove -destdir $(DESTDIR)/lib pa_ppx_runtime || true
	mkdir -p $(DESTDIR)/lib
	$(OCAMLFIND) install pa_ppx_runtime -destdir $(DESTDIR)/lib META $(TARGET) pa_ppx_runtime.cmx pa_ppx_runtime.cmi
	$(RM) -f META

local-install::
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

clean::
	rm -rf META camlp5.pa_ppx_runtime

$(TARGET): pa_ppx_runtime.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) pa_ppx_runtime.cmo -a -o $(TARGET)

$(TARGET:.cma=.cmxa): pa_ppx_runtime.cmx
	$(OCAMLFIND) ocamlopt $(DEBUG) pa_ppx_runtime.cmx -a -o $(TARGET:.cma=.cmxa)

$(TARGET): $(CMO)
$(TARGET:.cma=.cmxa): $(CMO:.cmo=.cmx)

depend::
	$(OCAMLFIND) ocamldep $(DEBUG) -package $(PACKAGES) -syntax camlp5r *.ml *.mli > .depend.NEW \
		&& mv .depend.NEW .depend

-include .depend
