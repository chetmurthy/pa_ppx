# Makefile,v
# Copyright (c) INRIA 2007-2017

TOP=.
include $(TOP)/config/Makefile

WD=$(shell pwd)
DESTDIR=
RM=rm

BASE_CMO = base/ppxutil.cmo base/pa_passthru.cmo base/pa_unmatched_vala.cmo
PAD_CMO =  pa_deriving/pa_deriving.cmo
PADP_CMO = pa_deriving/pa_deriving_show.cmo pa_deriving/pa_deriving_eq.cmo \
	pa_deriving/pa_deriving_ord.cmo pa_deriving/pa_deriving_enum.cmo

CMO = $(BASE_CMO) $(PAD_CMO) $(PADP_CMO)
CMI = $(CMO:.cmo=.cmi)

all:
	$(RM) -rf local-install && mkdir -p local-install/lib
	$(MAKE) -C base all
	$(MAKE) -C pa_unmatched_vala all
	$(MAKE) -C pa_deriving all
	$(MAKE) -C pa_deriving.plugins all
	$(MAKE) -C pa_import all
	$(MAKE) -C tests all



META: META.pl
	./META.pl > META

install: META.pl
	./META.pl $(DESTDIR)/pa_ppx > META
	$(OCAMLFIND) install -destdir $(DESTDIR) pa_ppx $(CMO) $(CMI) META

clean::
	rm -rf META local-install
	$(MAKE) -C base clean
	$(MAKE) -C pa_unmatched_vala clean
	$(MAKE) -C pa_deriving clean
	$(MAKE) -C pa_deriving.plugins clean
	$(MAKE) -C pa_import clean
	$(MAKE) -C tests clean

depend:
	$(MAKE) -C base depend
	$(MAKE) -C pa_unmatched_vala depend
	$(MAKE) -C pa_deriving depend
	$(MAKE) -C pa_deriving.plugins depend
	$(MAKE) -C pa_import depend
	$(MAKE) -C tests depend
