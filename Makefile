# Makefile,v
# Copyright (c) INRIA 2007-2017

TOP=.
include $(TOP)/config/Makefile

WD=$(shell pwd)
DESTDIR=
RM=rm

all:
	$(RM) -rf local-install && mkdir -p local-install/lib
	$(MAKE) -C util-lib all
	$(MAKE) -C runtime all
	$(MAKE) -C base all
	$(MAKE) -C pa_unmatched_vala all
	$(MAKE) -C pa_dock all
	$(MAKE) -C pa_here all
	$(MAKE) -C pa_undo_deriving all
	$(MAKE) -C pa_assert all
	$(MAKE) -C pa_inline_test all
	$(MAKE) -C pa_expect_test all
	$(MAKE) -C pa_hashrecons all
	$(MAKE) -C pa_deriving all
	$(MAKE) -C pa_deriving.plugins all
	$(MAKE) -C pa_import all
	$(MAKE) -C tests-ounit2 all
	$(MAKE) -C tests-inline all
	$(MAKE) -C tests-expect all

PACKAGES := pa_ppx_utils
PACKAGES := $(PACKAGES),pa_ppx_base
PACKAGES := $(PACKAGES),pa_ppx_unmatched_vala
PACKAGES := $(PACKAGES),pa_ppx_here
PACKAGES := $(PACKAGES),pa_ppx_assert
PACKAGES := $(PACKAGES),pa_ppx_inline_test
PACKAGES := $(PACKAGES),pa_ppx_expect_test
PACKAGES := $(PACKAGES),pa_ppx_deriving
PACKAGES := $(PACKAGES),pa_ppx_deriving_plugins.std
PACKAGES := $(PACKAGES),pa_ppx_deriving_plugins.yojson
PACKAGES := $(PACKAGES),pa_ppx_hashrecons
PACKAGES := $(PACKAGES),pa_ppx_import

camlp5o.pa_ppx:
	tools/LAUNCH $(MKCAMLP5) -verbose -package camlp5.pa_o,camlp5.pr_o,$(PACKAGES) $(KITS) -o $@


META: META.pl
	./META.pl > META

install: all META.pl camlp5o.pa_ppx
	$(OCAMLFIND) remove pa_ppx || true
	./META.pl > META
	$(OCAMLFIND) install pa_ppx META local-install/lib/*/*.* camlp5o.pa_ppx

uninstall:
	$(OCAMLFIND) remove pa_ppx || true

clean::
	rm -rf META local-install camlp5o.pa_ppx
	$(MAKE) -C util-lib clean
	$(MAKE) -C runtime clean
	$(MAKE) -C base clean
	$(MAKE) -C pa_unmatched_vala clean
	$(MAKE) -C pa_dock clean
	$(MAKE) -C pa_here clean
	$(MAKE) -C pa_undo_deriving clean
	$(MAKE) -C pa_assert clean
	$(MAKE) -C pa_inline_test clean
	$(MAKE) -C pa_expect_test clean
	$(MAKE) -C pa_hashrecons clean
	$(MAKE) -C pa_deriving clean
	$(MAKE) -C pa_deriving.plugins clean
	$(MAKE) -C pa_import clean
	$(MAKE) -C tests-ounit2 clean
	$(MAKE) -C tests-inline clean
	$(MAKE) -C tests-expect clean

depend:
	$(MAKE) -C util-lib depend
	$(MAKE) -C runtime depend
	$(MAKE) -C base depend
	$(MAKE) -C pa_unmatched_vala depend
	$(MAKE) -C pa_dock depend
	$(MAKE) -C pa_here depend
	$(MAKE) -C pa_undo_deriving depend
	$(MAKE) -C pa_assert depend
	$(MAKE) -C pa_inline_test depend
	$(MAKE) -C pa_expect_test depend
	$(MAKE) -C pa_hashrecons depend
	$(MAKE) -C pa_deriving depend
	$(MAKE) -C pa_deriving.plugins depend
	$(MAKE) -C pa_import depend
	$(MAKE) -C tests-ounit2 depend
	$(MAKE) -C tests-inline depend
	$(MAKE) -C tests-expect depend
