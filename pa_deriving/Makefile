# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=$(WD)/..
include $(TOP)/config/Makefile

DESTDIR=
RM=rm

LAUNCH=env TOP=$(TOP) $(TOP)/tools/LAUNCH
OCAMLFIND=$(LAUNCH) ocamlfind
MKCAMLP5=$(LAUNCH) mkcamlp5
PACKAGES := $(PACKAGES),pa_ppx_base

CMO=surveil.cmo pa_deriving.cmo
CMI=$(CMO:.cmo=.cmi)

all: $(CMO) camlp5.pa_ppx_pa_deriving local-install

KITS=$(CMO)

camlp5.pa_ppx_pa_deriving:
	$(MKCAMLP5) -verbose -package camlp5.pa_r,camlp5.pr_r,pa_ppx_base $(KITS) -o $@

META: META.pl
	./META.pl > META

install::
	./META.pl $(DESTDIR)/lib > META
	$(OCAMLFIND) remove -destdir $(DESTDIR)/lib pa_ppx_pa_deriving || true
	mkdir -p $(DESTDIR)/lib
	$(OCAMLFIND) install pa_ppx_pa_deriving -destdir $(DESTDIR)/lib $(CMO) $(CMI) META
	$(RM) -f META

local-install::
	$(MAKE) DESTDIR=$(TOP)/local-install/ install

clean::
	rm -rf META camlp5.pa_ppx_pa_deriving

surveil.cmo: pa_deriving.cmo
