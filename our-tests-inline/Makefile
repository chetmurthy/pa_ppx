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
NOT_OCAMLFIND=$(LAUNCH) not-ocamlfind
MKCAMLP5=$(LAUNCH) mkcamlp5
TESTRUNNER=$(TOP)/tests-inline/test-runner
PACKAGES := $(PACKAGES),ounit2,fmt,bos,pcre,rresult,compiler-libs.common,sexplib0,sexplib,str,threads.posix
OCAMLCFLAGS += -thread

CMO=
CMI := $(CMO:.cmo=.cmi)
CMX := $(CMO:.cmo=.cmx)

ORIGINAL_TESTS= \
	test_doc_comments_official_original.TEST \
	test_doc_comments_pa_dock_original.TEST \
	bigass_doc_comments_official_original.TEST \
	bigass_doc_comments_pa_dock_original.TEST \

REVISED_TESTS= \
	bigass_doc_comments_revised.TEST \
	test_doc_comments_revised.TEST \

TEST_EXECUTABLES = $(ORIGINAL_TESTS:.TEST=.pa.byte) $(REVISED_TESTS:.TEST=.pa.byte)

all: $(CMO) test-executables

test-executables: $(TEST_EXECUTABLES)

all-test: test
test: $(ORIGINAL_TESTS) $(REVISED_TESTS)

PAPACKAGES:=$(PACKAGEPREFIX)runtime
PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)import.syntax
PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)deriving_plugins.std.syntax
PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)deriving_plugins.sexp.syntax
PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)here.syntax
PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)assert.syntax
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

test_doc_comments_official_original.ml: test_doc_comments_original.ML
	cppo -n -D OFFICIAL $< > $@

test_doc_comments_pa_dock_original.ml: test_doc_comments_original.ML
	cppo -n -D PA_DOCK $< > $@

bigass_doc_comments_official_original.ml: bigass_doc_comments_original.ML
	cppo -n -D OFFICIAL $< > $@

bigass_doc_comments_pa_dock_original.ml: bigass_doc_comments_original.ML
	cppo -n -D PA_DOCK $< > $@

%_original.pa.byte: %_original.ml dock_test_utils.ml
	$(NOT_OCAMLFIND) preprocess -package $(PACKAGES),camlp5.pa_o,$(PAINLINEPACKAGES) -syntax camlp5o $(PAINLINEOPT) $< | $(OCAMLFIND) camlp5/papr_official.exe -binary-input -impl > $<.pappx.ppo
	$(OCAMLFIND) ocamlc $(DEBUG) -custom -thread $(WARNERR) -package $(PACKAGES),camlp5.pa_o,$(PAINLINEPACKAGES) -syntax camlp5o $(PAINLINEOPT) -I ../tests-ounit2 -linkall -linkpkg ../tests-ounit2/testutil.cmo dock_test_utils.ml $< ppx_inline_test_epilogue.ml -o $@

%_revised.pa.byte: %_revised.ml dock_test_utils.ml
	$(NOT_OCAMLFIND) preprocess -package $(PACKAGES),camlp5.pa_r,$(PAINLINEPACKAGES) -syntax camlp5o $(PAINLINEOPT) $< | $(OCAMLFIND) camlp5/papr_official.exe -binary-input -impl > $<.pappx.ppo
	$(OCAMLFIND) ocamlc $(DEBUG) -custom -thread $(WARNERR) -package $(PACKAGES),camlp5.pa_r,$(PAINLINEPACKAGES) -syntax camlp5o $(PAINLINEOPT) -I ../tests-ounit2 -linkall -linkpkg ../tests-ounit2/testutil.cmo dock_test_utils.ml $< ppx_inline_test_epilogue.ml -o $@

%.TEST: %.pa.byte
	$(TESTRUNNER) $^ foo $(VERBOSE)

.SUFFIXES: .TEST

clean::
	rm -f *.corrected *.ppo *_doc_comments_official.ml *_doc_comments_pa_dock.ml

EXTERNAL := $(shell $(OCAMLFIND) query -predicates byte $(PACKAGES),$(PAINLINEPACKAGES))
$(TEST_EXECUTABLES): $(EXTERNAL)

depend::
	$(OCAMLFIND) ocamldep $(DEBUG) $(WARNERR) -package $(PACKAGES) -syntax camlp5o test_*.ml > .depend.NEW
	mv .depend.NEW .depend

-include .depend
