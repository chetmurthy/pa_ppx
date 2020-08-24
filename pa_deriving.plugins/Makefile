# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile

DESTDIR=
RM=rm

LAUNCH=env TOP=$(TOP) $(TOP)/tools/LAUNCH
OCAMLFIND=$(LAUNCH) ocamlfind
NOT_OCAMLFIND=$(LAUNCH) not-ocamlfind
MKCAMLP5=$(LAUNCH) mkcamlp5
PACKAGES := $(PACKAGES),pa_ppx_deriving,pa_ppx_base,pa_ppx_unmatched_vala

ML=\
	pa_deriving_show.ml \
	pa_deriving_eq.ml \
	pa_deriving_ord.ml \
	pa_deriving_enum.ml \
	pa_deriving_iter.ml \
	pa_deriving_map.ml \
	pa_deriving_rewrite.ml \
	pa_deriving_fold.ml \
	pa_deriving_make.ml \
	pa_deriving_yojson.ml \
	pa_deriving_sexp.ml \
	pa_deriving_protobuf.ml \

CMO := $(ML:.ml=.cmo)
CMI := $(ML:.ml=.cmi)
CMX := $(ML:.ml=.cmx)
CMT := $(ML:.ml=.cmt)
CMTI := $(MLI:.mli=.cmti)
BATCHTOP=camlp5o.pa_ppx_deriving_plugins camlp5o.pa_ppx_deriving_plugins.opt \
	camlp5r.pa_ppx_deriving_plugins camlp5r.pa_ppx_deriving_plugins.opt

all: $(CMO) $(BATCHTOP) local-install

doc: $(CMT) $(CMTI)

KITS=$(CMO)

camlp5r.pa_ppx_deriving_plugins: $(KITS)
	$(MKCAMLP5) -verbose -package camlp5.pa_r,camlp5.pr_r,pa_ppx_deriving $(KITS) -o $@

camlp5r.pa_ppx_deriving_plugins.opt: $(KITS:.cmo=.cmx)
	$(MKCAMLP5).opt -verbose -package camlp5.pa_r,camlp5.pr_r,pa_ppx_deriving $(KITS:.cmo=.cmx) -o $@

camlp5o.pa_ppx_deriving_plugins: $(KITS)
	$(MKCAMLP5) -verbose -package camlp5.pa_o,camlp5.pr_o,pa_ppx_deriving $(KITS) -o $@

camlp5o.pa_ppx_deriving_plugins.opt: $(KITS:.cmo=.cmx)
	$(MKCAMLP5).opt -verbose -package camlp5.pa_o,camlp5.pr_o,pa_ppx_deriving $(KITS:.cmo=.cmx) -o $@

META: META.pl
	./META.pl > META

install:: $(BATCHTOP)
	mkdir -p $(DESTDIR)/lib
	./META.pl $(DESTDIR)/lib > META
	$(NOT_OCAMLFIND) reinstall-if-diff pa_ppx_deriving_plugins -destdir $(DESTDIR)/lib META $(CMO) $(CMX) $(MLI:.mli=.cmi) $(KITS:.cmo=.o) $(wildcard *.cmt*) $(BATCHTOP)
	$(RM) -f META

local-install::
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

clean::
	rm -rf META camlp5*.pa_ppx_deriving_plugins*

EXTERNAL := $(shell $(OCAMLFIND) query -predicates byte $(PACKAGES))
$(CMO): $(EXTERNAL)

depend::
	$(OCAMLFIND) ocamldep $(DEBUG) -package $(PACKAGES) -syntax camlp5r *.ml *.mli > .depend.NEW \
		&& mv .depend.NEW .depend

-include .depend
