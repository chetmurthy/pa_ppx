# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=$(WD)/..
include $(TOP)/config/Makefile

PACKAGEPREFIX=pa_ppx_

DESTDIR=
RM=rm

LAUNCH=env TOP=$(TOP) $(TOP)/tools/LAUNCH
OCAMLFIND=$(LAUNCH) ocamlfind
MKCAMLP5=$(LAUNCH) mkcamlp5
PACKAGES := $(PACKAGES),ounit2,fmt,pcre,rresult,compiler-libs.common

CMO=
CMI=$(CMO:.cmo=.cmi)

TESTS= \
	test_inline_test.TEST failures.TEST order.TEST random_state.TEST \

all: $(CMO) test-executables

test-executables: $(TESTS:.TEST=.pa.byte) $(TESTS:.TEST=.ppx.byte)

test: $(TESTS)

PPXPACKAGES:=ppx_import
PPXPACKAGES:=$(PPXPACKAGES),ppx_deriving.std
PPXPACKAGES:=$(PPXPACKAGES),ppx_here


PPXINLINEPACKAGES:=ppx_inline_test.runtime-lib
PPXINLINEPACKAGES:=$(PPXINLINEPACKAGES),$(PPXPACKAGES)
PPXINLINEPACKAGES:=$(PPXINLINEPACKAGES),ppx_expect
PPXINLINEPACKAGES:=$(PPXINLINEPACKAGES),ppx_inline_test.runner.lib
PPXINLINEPACKAGES:=$(PPXINLINEPACKAGES),ppx_expect.evaluator

PPXINLINEOPT = -ppxopt ppx_expect,-inline-test-lib  -ppxopt ppx_expect,foo

PAPACKAGES:=$(PACKAGEPREFIX)runtime
PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)import.syntax
PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)deriving_plugins.std.syntax
PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)here.syntax

PAINLINEPACKAGES:=$(PAPACKAGES)
PAINLINEPACKAGES:=$(PAINLINEPACKAGES),$(PACKAGEPREFIX)inline_test.syntax
PAINLINEPACKAGES:=$(PAINLINEPACKAGES),$(PACKAGEPREFIX)expect_test.syntax
PAINLINEPACKAGES:=$(PAINLINEPACKAGES),ppx_expect.evaluator
PAINLINEPACKAGES:=$(PAINLINEPACKAGES),ppx_inline_test.runtime-lib,ppx_inline_test.runner.lib

PAINLINEOPT=-ppopt -pa_inline_test-lib -ppopt foo

%.ppx.byte: %.ml
	$(OCAMLFIND) ocamlc $(DEBUG) -custom $(WARNERR) -package $(PACKAGES),$(PPXINLINEPACKAGES) $(PPXINLINEOPT) -linkall -linkpkg $< ppx_inline_test_epilogue.ml -o $@

%.pa.byte: %.ml
	$(OCAMLFIND) ocamlc $(DEBUG) -custom $(WARNERR) -package $(PACKAGES),$(PAINLINEPACKAGES),camlp5.pr_o.syntax -syntax camlp5o $(PAINLINEOPT) -linkall -linkpkg $< ppx_inline_test_epilogue.ml -o $@

%.TEST: %.pa.byte %.ppx.byte
	./test-runner $^ foo $(VERBOSE)

.SUFFIXES: .TEST

clean::
	rm -f *.corrected

depend::
	$(OCAMLFIND) ocamldep $(DEBUG) $(WARNERR) -package $(PACKAGES) -syntax camlp5o test_*.ml > .depend.NEW
	mv .depend.NEW .depend

-include .depend
