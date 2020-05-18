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
TESTRUNNER=$(TOP)/tests-inline/test-runner
PACKAGES := $(PACKAGES),ounit2,fmt,bos,pcre,rresult,compiler-libs.common,sexplib0,sexplib,str,threads.posix
OCAMLCFLAGS += -thread

CMO=
CMI=$(CMO:.cmo=.cmi)

TESTS= \
	test_doc_comments_official.TEST \
	test_doc_comments_pa_dock.TEST \

BUSTED_TESTS= \
	busted_doc_comments_official.TEST \
	busted_doc_comments_pa_dock.TEST \

all: $(CMO) test-executables

test-executables: $(TESTS:.TEST=.pa.byte) $(BUSTED_TESTS:.TEST=.pa.byte)

all-test: test busted-test
test: $(TESTS)
busted-test: $(BUSTED_TESTS)

PAPACKAGES:=$(PACKAGEPREFIX)runtime
PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)import.syntax
PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)deriving_plugins.std.syntax
PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)deriving_plugins.sexp.syntax
PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)here.syntax
PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)assert.syntax
PAPACKAGES:=$(PAPACKAGES),camlp5.pa_o
PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)dock

PAINLINEPACKAGES:=$(PAPACKAGES)
PAINLINEPACKAGES:=$(PAINLINEPACKAGES),$(PACKAGEPREFIX)inline_test.syntax
PAINLINEPACKAGES:=$(PAINLINEPACKAGES),$(PACKAGEPREFIX)expect_test.syntax
PAINLINEPACKAGES:=$(PAINLINEPACKAGES),expect_test_helpers
PAINLINEPACKAGES:=$(PAINLINEPACKAGES),ppx_expect.matcher
PAINLINEPACKAGES:=$(PAINLINEPACKAGES),ppx_expect.collector
PAINLINEPACKAGES:=$(PAINLINEPACKAGES),ppx_expect.evaluator
PAINLINEPACKAGES:=$(PAINLINEPACKAGES),ppx_inline_test.runtime-lib,ppx_inline_test.runner.lib

PAINLINEOPT=-ppopt -pa_inline_test-lib -ppopt foo

test_doc_comments_official.ml: test_doc_comments.ML
	cppo -n -D OFFICIAL $< > $@

test_doc_comments_pa_dock.ml: test_doc_comments.ML
	cppo -n -D PA_DOCK $< > $@

busted_doc_comments_official.ml: busted_doc_comments.ML
	cppo -n -D OFFICIAL $< > $@

busted_doc_comments_pa_dock.ml: busted_doc_comments.ML
	cppo -n -D PA_DOCK $< > $@

%.pa.byte: %.ml
	$(OCAMLFIND)2 preprocess -package $(PACKAGES),$(PAINLINEPACKAGES) -syntax camlp5o $(PAINLINEOPT) $< | $(OCAMLFIND) camlp5/papr_official.exe -binary-input -impl > $<.pappx.ppo
	$(OCAMLFIND) ocamlc $(DEBUG) -custom -thread $(WARNERR) -package $(PACKAGES),$(PAINLINEPACKAGES) -syntax camlp5o $(PAINLINEOPT) -I ../tests-ounit2 -linkall -linkpkg ../tests-ounit2/testutil.cmo $< ppx_inline_test_epilogue.ml -o $@

%.TEST: %.pa.byte
	$(TESTRUNNER) $^ foo $(VERBOSE)

.SUFFIXES: .TEST

clean::
	rm -f *.corrected *.ppo test_doc_comments_official.ml test_doc_comments_pa_dock.ml

depend::
	$(OCAMLFIND) ocamldep $(DEBUG) $(WARNERR) -package $(PACKAGES) -syntax camlp5o test_*.ml > .depend.NEW
	mv .depend.NEW .depend

-include .depend
