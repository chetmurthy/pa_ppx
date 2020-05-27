# Makefile,v
# Copyright (c) INRIA 2007-2017

TOP=.
include $(TOP)/config/Makefile

WD=$(shell pwd)
DESTDIR=
RM=rm

SYSDIRS= util-lib runtime base pa_unmatched_vala \
	pa_dock pa_here pa_undo_deriving pa_assert \
	pa_inline_test pa_expect_test pa_hashrecons \
	pa_deriving pa_deriving.plugins pa_import \

TESTDIRS= tests-ounit2 tests-inline tests-expect our-tests-inline

all:
	set -e; for i in $(SYSDIRS) $(TESTDIRS); do cd $$i; $(MAKE) all; cd ..; done

doc: all
	set -e; for i in $(SYSDIRS); do cd $$i; $(MAKE) doc; cd ..; done
	rm -rf docs
	tools/make-docs pa_ppx docs

test: all
	set -e; for i in $(TESTDIRS); do cd $$i; $(MAKE) test; cd ..; done

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
	set -e; for i in $(SYSDIRS) $(TESTDIRS); do cd $$i; $(MAKE) clean; cd ..; done
	rm -rf docs

depend:
	set -e; for i in $(SYSDIRS) $(TESTDIRS); do cd $$i; $(MAKE) depend; cd ..; done
