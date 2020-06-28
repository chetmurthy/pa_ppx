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
PACKAGES := $(PACKAGES),ounit2,fmt,pcre,rresult,compiler-libs.common,sexplib0

CMO=
CMI := $(CMO:.cmo=.cmi)
CMX := $(CMO:.cmo=.cmx)

TESTS= \
	test_inline_test.byte \
	test_inline_test.ppx.byte \

all: $(CMO) $(TESTS)

test: $(TESTS:.byte=)

% : %.byte
	./$<

PPXPACKAGES:=ppx_import
PPXPACKAGES:=$(PPXPACKAGES),ppx_deriving.show
PPXPACKAGES:=$(PPXPACKAGES),ppx_deriving.eq
PPXPACKAGES:=$(PPXPACKAGES),ppx_deriving.ord
PPXPACKAGES:=$(PPXPACKAGES),ppx_deriving.enum
PPXPACKAGES:=$(PPXPACKAGES),ppx_deriving.iter
PPXPACKAGES:=$(PPXPACKAGES),ppx_deriving.map
PPXPACKAGES:=$(PPXPACKAGES),ppx_deriving.fold
PPXPACKAGES:=$(PPXPACKAGES),ppx_deriving.make
PPXPACKAGES:=$(PPXPACKAGES),ppx_deriving_yojson
PPXPACKAGES:=$(PPXPACKAGES),ppx_here


PPXINLINEPACKAGES:=ppx_inline_test.runtime-lib
PPXINLINEPACKAGES:=$(PPXINLINEPACKAGES),$(PPXPACKAGES)
PPXINLINEPACKAGES:=$(PPXINLINEPACKAGES),ppx_expect
PPXINLINEPACKAGES:=$(PPXINLINEPACKAGES),ppx_inline_test.runner.lib
PPXINLINEPACKAGES:=$(PPXINLINEPACKAGES),ppx_expect.evaluator

PPXINLINEOPT = -ppxopt ppx_expect,-inline-test-lib  -ppxopt ppx_expect,foo

PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)import
PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)deriving_plugins.std
PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)deriving_plugins.yojson
PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)hashrecons
PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)here

PAINLINEPACKAGES:=$(PAPACKAGES)
PAINLINEPACKAGES:=$(PAINLINEPACKAGES),$(PACKAGEPREFIX)expect_test
PAINLINEPACKAGES:=$(PAINLINEPACKAGES),$(PACKAGEPREFIX)inline_test
PAINLINEPACKAGES:=$(PAINLINEPACKAGES),ppx_expect.evaluator

test_inline_test.ppx.byte: test_inline_test.ml
	$(OCAMLFIND) ocamlc $(DEBUG) -custom $(WARNERR) -package $(PACKAGES),ounit2,$(PPXINLINEPACKAGES) $(PPXINLINEOPT) -linkall -linkpkg $< ppx_inline_test_epilogue.ml -o $@

test_inline_test.ppx: test_inline_test.ppx.byte
	./$< inline-test-runner foo -source-tree-root . -diff-cmd - || \
	/usr/bin/git diff --no-index -u test_inline_test.ml test_inline_test.ml.corrected | cat


PAINLINEOPT=-ppopt -pa_inline_test-lib -ppopt foo
test_inline_test.byte: test_inline_test.ml
	$(OCAMLFIND) ocamlc $(DEBUG) -custom $(WARNERR) -package $(PACKAGES),ounit2,$(PAINLINEPACKAGES),camlp5.pr_o,ppx_inline_test.runtime-lib,ppx_inline_test.runner.lib -syntax camlp5o $(PAINLINEOPT) -linkall -linkpkg $< ppx_inline_test_epilogue.ml -o $@

test_inline_test: test_inline_test.byte
	./$< inline-test-runner foo -verbose -require-tag js-only -source-tree-root . -diff-cmd - || \
	/usr/bin/git diff --no-index -u test_inline_test.ml test_inline_test.ml.corrected | cat


clean::
	rm -f *.corrected

EXTERNAL := $(shell $(OCAMLFIND) query -predicates byte $(PACKAGES),$(PAINLINEPACKAGES))
$(TESTS): $(EXTERNAL)

depend::
	$(OCAMLFIND) ocamldep $(DEBUG) $(WARNERR) -package $(PACKAGES) -syntax camlp5o test_*.ml > .depend.NEW
	mv .depend.NEW .depend

-include .depend
