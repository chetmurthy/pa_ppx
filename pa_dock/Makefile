# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile

DESTDIR=
RM=rm

LAUNCH=env TOP=$(TOP) $(TOP)/tools/LAUNCH
OCAMLFIND=$(LAUNCH) ocamlfind
MKCAMLP5=$(LAUNCH) mkcamlp5

PACKAGES := $(PACKAGES),fmt,pa_ppx_base,camlp5.pa_r.syntax,camlp5.lexer.syntax
TARGET := pa_ppx_dock.cma
CMO := comment_lexer.cmo
CMI := $(CMO:.cmo=.cmi)
SYNTAX := camlp5r
OCAMLCFLAGS := $(OCAMLCFLAGS) -for-pack Pa_ppx_dock

all: $(TARGET) $(TARGET:.cma=.cmxa) camlp5.pa_ppx_dock local-install

pa_ppx_dock.cmo: $(CMO)
	$(OCAMLFIND) ocamlc $(DEBUG) -pack -o pa_ppx_dock.cmo $^

pa_ppx_dock.cmx: $(CMO:.cmo=.cmx)
	$(OCAMLFIND) ocamlopt $(DEBUG) -pack -o pa_ppx_dock.cmx $^

camlp5.pa_ppx_dock: $(TARGET)
	$(MKCAMLP5) -verbose -package fmt,camlp5.pa_r,camlp5.pr_r,pa_ppx_base $(TARGET) -o $@

META: META.pl
	./META.pl > META
install::
	./META.pl $(DESTDIR)/lib > META
	$(OCAMLFIND) remove -destdir $(DESTDIR)/lib pa_ppx_dock || true
	mkdir -p $(DESTDIR)/lib
	$(OCAMLFIND) install pa_ppx_dock -destdir $(DESTDIR)/lib META $(TARGET) $(TARGET:.cma=.cmxa) $(TARGET:.cma=.cmi)
	$(RM) -f META

local-install::
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

clean::
	rm -rf META camlp5.pa_ppx_dock

$(TARGET): $(TARGET:.cma=.cmo)
	$(OCAMLFIND) ocamlc $(DEBUG) $(TARGET:.cma=.cmo) -a -o $(TARGET)

$(TARGET:.cma=.cmxa): $(TARGET:.cma=.cmx)
	$(OCAMLFIND) ocamlopt $(DEBUG) $(TARGET:.cma=.cmx) -a -o $(TARGET:.cma=.cmxa)

$(TARGET): $(CMO)
$(TARGET:.cma=.cmxa): $(CMO:.cmo=.cmx)

EXTERNAL := $(shell $(OCAMLFIND) query -predicates byte $(PACKAGES))
$(CMO): $(EXTERNAL)

depend::
	$(OCAMLFIND) ocamldep $(DEBUG) -package $(PACKAGES) -syntax camlp5r *.ml *.mli > .depend.NEW \
		&& mv .depend.NEW .depend

-include .depend
