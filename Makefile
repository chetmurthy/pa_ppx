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
	protobuf_runtime

TESTDIRS= tests-ounit2 our-tests-inline tests-deriving-protobuf \
	 tests-inline tests-expect

all: sys
	set -e; for i in $(TESTDIRS); do cd $$i; $(MAKE) all; cd ..; done
	$(MAKE) camlp5o.pa_ppx camlp5o.pa_ppx.opt

sys: prereqs
	set -e; for i in $(SYSDIRS); do cd $$i; $(MAKE) all; cd ..; done

doc: all
	set -e; for i in $(SYSDIRS); do cd $$i; $(MAKE) doc; cd ..; done
	rm -rf docs
	tools/make-docs pa_ppx docs
	make -C doc html

test: all
	set -e; for i in $(TESTDIRS); do cd $$i; $(MAKE) test; cd ..; done

prereqs:
	(perl -MIPC::System::Simple -e 1 > /dev/null 2>&1) || (echo "MUST install Perl module IPC::System::Simple" && exit -1)
	(perl -MString::ShellQuote -e 1 > /dev/null 2>&1) || (echo "MUST install Perl module String::ShellQuote" && exit -1)


bootstrap:
	$(MAKE) -C runtime bootstrap-exn bootstrap-exn-i
	$(MAKE) -C base bootstrap-pp-MLast bootstrap-pp-MLast-i bootstrap-pp-parsetree bootstrap-pp-parsetree-i

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
	tools/LAUNCH $(MKCAMLP5) -verbose -package camlp5.pa_o,camlp5.pr_o,$(PACKAGES) -o $@

camlp5o.pa_ppx.opt:
	tools/LAUNCH $(MKCAMLP5).opt -verbose -package camlp5.pa_o,camlp5.pr_o,$(PACKAGES) -o $@

META: META.pl
	./META.pl > META

install: sys META.pl
	$(OCAMLFIND) remove pa_ppx || true
	./META.pl > META
	$(OCAMLFIND) install pa_ppx META local-install/lib/*/*.* camlp5o.pa_ppx camlp5o.pa_ppx.opt

uninstall:
	$(OCAMLFIND) remove pa_ppx || true

clean::
	set -e; for i in $(SYSDIRS) $(TESTDIRS); do cd $$i; $(MAKE) clean; cd ..; done
	rm -rf docs local-install

depend:
	set -e; for i in $(SYSDIRS) $(TESTDIRS); do cd $$i; $(MAKE) depend; cd ..; done
