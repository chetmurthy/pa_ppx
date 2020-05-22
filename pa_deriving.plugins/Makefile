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

CMO=\
	pa_deriving_show.cmo \
	pa_deriving_eq.cmo \
	pa_deriving_ord.cmo \
	pa_deriving_enum.cmo \
	pa_deriving_iter.cmo \
	pa_deriving_map.cmo \
	pa_deriving_fold.cmo \
	pa_deriving_make.cmo \
	pa_deriving_yojson.cmo \
	pa_deriving_sexp.cmo \

CMI := $(CMO:.cmo=.cmi)
CMX := $(CMO:.cmo=.cmx)

all: $(CMO) camlp5.pa_ppx_deriving_plugins local-install

KITS=$(CMO)

camlp5.pa_ppx_deriving_plugins:
	$(MKCAMLP5) -verbose -package camlp5.pa_r,camlp5.pr_r,pa_ppx_deriving $(KITS) -o $@

META: META.pl
	./META.pl > META

install::
	mkdir -p $(DESTDIR)/lib
	./META.pl $(DESTDIR)/lib > META
	$(NOT_OCAMLFIND) reinstall-if-diff pa_ppx_deriving_plugins -destdir $(DESTDIR)/lib META $(CMO) $(CMI)
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
