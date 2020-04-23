# Makefile,v
# Copyright (c) INRIA 2007-2017

TOP=.
include $(TOP)/config/Makefile

WD=$(shell pwd)
DESTDIR=
RM=rm

all:
	$(RM) -rf local-install && mkdir -p local-install/lib
	$(MAKE) -C base all
	$(MAKE) -C pa_unmatched_vala all
	$(MAKE) -C pa_hashrecons all
	$(MAKE) -C pa_deriving all
	$(MAKE) -C pa_deriving.plugins all
	$(MAKE) -C pa_import all
	$(MAKE) -C tests all



META: META.pl
	./META.pl > META

install: all META.pl
	$(OCAMLFIND) remove pa_ppx || true
	./META.pl > META
	$(OCAMLFIND) install pa_ppx local-install/lib/*/*.* META

uninstall:
	$(OCAMLFIND) remove pa_ppx || true

clean::
	rm -rf META local-install
	$(MAKE) -C base clean
	$(MAKE) -C pa_unmatched_vala clean
	$(MAKE) -C pa_hashrecons clean
	$(MAKE) -C pa_deriving clean
	$(MAKE) -C pa_deriving.plugins clean
	$(MAKE) -C pa_import clean
	$(MAKE) -C tests clean

depend:
	$(MAKE) -C base depend
	$(MAKE) -C pa_unmatched_vala depend
	$(MAKE) -C pa_hashrecons depend
	$(MAKE) -C pa_deriving depend
	$(MAKE) -C pa_deriving.plugins depend
	$(MAKE) -C pa_import depend
	$(MAKE) -C tests depend
