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
PACKAGES := $(PACKAGES),pa_ppx_pa_deriving

CMO=\
	pa_deriving_show.cmo \
	pa_deriving_eq.cmo \
	pa_deriving_ord.cmo \
	pa_deriving_enum.cmo \
	pa_deriving_iter.cmo \
	pa_deriving_map.cmo \


#	pa_deriving_fold.cmo \

CMI=$(CMO:.cmo=.cmi)

all: $(CMO) camlp5.pa_ppx_pa_deriving_plugins local-install

KITS=$(CMO)

camlp5.pa_ppx_pa_deriving_plugins:
	$(MKCAMLP5) -verbose -package camlp5.pa_r,camlp5.pr_r,pa_ppx_pa_deriving $(KITS) -o $@

META: META.pl
	./META.pl > META

install::
	./META.pl $(DESTDIR)/lib > META
	$(OCAMLFIND) remove -destdir $(DESTDIR)/lib pa_ppx_pa_deriving_plugins || true
	mkdir -p $(DESTDIR)/lib
	$(OCAMLFIND) install pa_ppx_pa_deriving_plugins -destdir $(DESTDIR)/lib META $(CMO) $(CMI)
	$(RM) -f META

local-install::
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

clean::
	rm -rf META camlp5.pa_ppx_pa_deriving_plugins
