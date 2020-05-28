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
PACKAGES := $(PACKAGES),pa_ppx_deriving,pa_ppx_base.syntax,pa_ppx_unmatched_vala.syntax

ML=\
	pa_deriving_show.ml \
	pa_deriving_eq.ml \
	pa_deriving_ord.ml \
	pa_deriving_enum.ml \
	pa_deriving_iter.ml \
	pa_deriving_map.ml \
	pa_deriving_fold.ml \
	pa_deriving_make.ml \
	pa_deriving_yojson.ml \
	pa_deriving_sexp.ml \

CMO := $(ML:.ml=.cmo)
CMI := $(ML:.ml=.cmi)
CMX := $(ML:.ml=.cmx)
CMT := $(ML:.ml=.cmt)
CMTI := $(MLI:.mli=.cmti)

all: $(CMO) camlp5.pa_ppx_deriving_plugins local-install

doc: $(CMT) $(CMTI)

KITS=$(CMO)

camlp5.pa_ppx_deriving_plugins:
	$(MKCAMLP5) -verbose -package camlp5.pa_r,camlp5.pr_r,pa_ppx_deriving $(KITS) -o $@

META: META.pl
	./META.pl > META

install::
	mkdir -p $(DESTDIR)/lib
	./META.pl $(DESTDIR)/lib > META
	$(NOT_OCAMLFIND) reinstall-if-diff pa_ppx_deriving_plugins -destdir $(DESTDIR)/lib META $(CMO) $(MLI:.mli=.cmi) $(wildcard *.cmt*)
	$(RM) -f META

local-install::
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

clean::
	rm -rf META camlp5.pa_ppx_deriving_plugins

EXTERNAL := $(shell $(OCAMLFIND) query -predicates byte $(PACKAGES))
$(CMO): $(EXTERNAL)

depend::
	$(OCAMLFIND) ocamldep $(DEBUG) -package $(PACKAGES) -syntax camlp5r *.ml *.mli > .depend.NEW \
		&& mv .depend.NEW .depend

-include .depend
