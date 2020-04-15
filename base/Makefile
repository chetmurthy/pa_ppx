# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=$(WD)/..
include $(TOP)/config/Makefile

DESTDIR=
RM=rm

LAUNCH=env TOP=$(TOP) $(TOP)/tools/LAUNCH
OCAMLFIND=$(LAUNCH) ocamlfind

CMO=ppxutil.cmo pa_passthru.cmo pa_unmatched_vala.cmo
CMI=$(CMO:.cmo=.cmi)

all: $(CMO) camlp5.pa_ppx_base local-install

KITS=$(CMO)

camlp5.pa_ppx_base:
	$(MKCAMLP5) -verbose -package camlp5.pa_r,camlp5.pr_r $(KITS) -o $@

META: META.pl
	./META.pl > META
install::
	./META.pl $(DESTDIR)/lib > META
	$(OCAMLFIND) remove -destdir $(DESTDIR)/lib pa_ppx_base || true
	mkdir -p $(DESTDIR)/lib
	$(OCAMLFIND) install pa_ppx_base -destdir $(DESTDIR)/lib $(CMO) $(CMI) META
	$(RM) -f META

local-install::
	$(MAKE) DESTDIR=$(TOP)/local-install/ install

clean::
	rm -rf META camlp5.pa_ppx_base
