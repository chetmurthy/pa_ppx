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
	test_deriving_show.byte \
	test_deriving_show.ppx.byte \
	test_deriving_eq.byte \
	test_deriving_eq.ppx.byte \
	test_deriving_ord.byte \
	test_deriving_ord.ppx.byte \
	test_deriving_enum.byte \
	test_deriving_enum.ppx.byte \
	test_deriving_iter.byte \
	test_deriving_iter.ppx.byte \
	test_deriving_map.byte \
	test_deriving_map.ppx.byte \
	test_deriving_fold.byte \
	test_deriving_fold.ppx.byte \
	test_deriving_make.byte \
	test_deriving_make.ppx.byte \
	test_here.byte \
	test_here.ppx.byte \
	test_import0.byte \
	test_import_camlp5.byte \
	test_ppx_import.byte \
	test_ppx_import.ppx.byte \
	test_hashrecons.byte \
	test_passthru_hashrecons.byte \
	test_inline_test.byte \
	test_inline_test.ppx.byte \
	test_ppx_yojson.byte \
	test_ppx_yojson.ppx.byte \

#	test_inline_test.byte \


all: $(CMO) $(TESTS) test

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

PAPACKAGES:=$(PACKAGEPREFIX)runtime
PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)import.syntax
PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)deriving_plugins.std.syntax
PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)deriving_plugins.yojson.syntax
PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)hashrecons.syntax
PAPACKAGES:=$(PAPACKAGES),$(PACKAGEPREFIX)here.syntax

PAINLINEPACKAGES:=$(PAPACKAGES)
PAINLINEPACKAGES:=$(PAINLINEPACKAGES),$(PACKAGEPREFIX)expect_test.syntax
PAINLINEPACKAGES:=$(PAINLINEPACKAGES),$(PACKAGEPREFIX)inline_test.syntax
PAINLINEPACKAGES:=$(PAINLINEPACKAGES),ppx_expect.evaluator

test_%.byte: test_%.ml testutil.cmo testutil2.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) -package $(PACKAGES),yojson,$(PAPACKAGES) \
	-linkall -linkpkg -syntax camlp5o testutil.cmo testutil2.cmo $< -o $@

test_%.ppx.byte: test_%.ml testutil.cmo testutil2.cmo stuff.cmo test_self_import.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) -custom $(WARNERR) -package $(PACKAGES),ounit2,$(PPXPACKAGES) -linkall -linkpkg testutil.cmo testutil2.cmo stuff.cmo test_self_import.cmo $< -o $@

test_inline_test.ppx.byte: test_inline_test.ml
	$(OCAMLFIND) ocamlc $(DEBUG) -custom $(WARNERR) -package $(PACKAGES),ounit2,$(PPXINLINEPACKAGES) $(PPXINLINEOPT) -linkall -linkpkg $< ppx_inline_test_epilogue.ml -o $@

test_inline_test.ppx: test_inline_test.ppx.byte
	./$< inline-test-runner foo -source-tree-root . -diff-cmd - || \
	/usr/bin/git diff --no-index -u test_inline_test.ml test_inline_test.ml.corrected | cat


PAINLINEOPT=-ppopt -pa_inline_test-lib -ppopt foo
test_inline_test.byte: test_inline_test.ml testutil.cmo testutil2.cmo stuff.cmo test_self_import.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) -custom $(WARNERR) -package $(PACKAGES),ounit2,$(PAINLINEPACKAGES),camlp5.pr_o.syntax,ppx_inline_test.runtime-lib,ppx_inline_test.runner.lib -syntax camlp5o $(PAINLINEOPT) -linkall -linkpkg testutil.cmo testutil2.cmo stuff.cmo test_self_import.cmo $< ppx_inline_test_epilogue.ml -o $@

test_inline_test: test_inline_test.byte
	./$< inline-test-runner foo -verbose -require-tag js-only -source-tree-root . -diff-cmd - || \
	/usr/bin/git diff --no-index -u test_inline_test.ml test_inline_test.ml.corrected | cat

test_boo.ppx.byte: test_boo.ml testutil.cmo testutil2.cmo stuff.cmo test_self_import.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) -custom $(WARNERR) -package $(PACKAGES),ounit2,$(PPXINLINEPACKAGES) $(PPXINLINEOPT) -linkall -linkpkg testutil.cmo testutil2.cmo stuff.cmo test_self_import.cmo $< ppx_inline_test_epilogue.ml -o $@

test_boo.ppx: test_boo.ppx.byte
	./$< inline-test-runner foo -verbose -source-tree-root . -diff-cmd - || \
	/usr/bin/git diff --no-index -u test_boo.ml test_boo.ml.corrected | cat

PAINLINEOPT=-ppopt -pa_inline_test-lib -ppopt foo
test_boo.byte: test_boo.ml testutil.cmo testutil2.cmo stuff.cmo test_self_import.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) -custom $(WARNERR) -package $(PACKAGES),ounit2,$(PAINLINEPACKAGES),camlp5.pr_o.syntax,ppx_inline_test.runtime-lib,ppx_inline_test.runner.lib -syntax camlp5o $(PAINLINEOPT) -linkall -linkpkg testutil.cmo testutil2.cmo stuff.cmo test_self_import.cmo $< ppx_inline_test_epilogue.ml -o $@

test_boo: test_boo.byte
	./$< inline-test-runner foo -verbose -source-tree-root . -diff-cmd - || \
	/usr/bin/git diff --no-index -u test_boo.ml test_boo.ml.corrected | cat



test_passthru_hashrecons.byte: test_passthru_hashrecons.ml testutil.cmo testutil2.cmo hCPassthru.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) -package $(PACKAGES),camlp5.pa_o,camlp5.pr_o,$(PACKAGEPREFIX)base,$(PACKAGEPREFIX)hashrecons.syntax \
	-linkall -linkpkg -syntax camlp5o testutil.cmo testutil2.cmo hCPassthru.cmo $< -o $@

hCPassthru.cmo: hCPassthru.ml
	$(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) -package $(PACKAGES),$(PACKAGEPREFIX)base,$(PACKAGEPREFIX)hashrecons.syntax -syntax camlp5r -c $<

test_import0.byte: test_import0.ml testutil.cmo testutil2.cmo stuff.cmo test_self_import.cmi
	$(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) -package $(PACKAGES),$(PACKAGEPREFIX)import.syntax \
	-ppopt -pa_import-package -ppopt $(PACKAGES) \
	-ppopt -pa_import-I -ppopt . \
	-linkall -linkpkg -syntax camlp5o testutil.cmo testutil2.cmo $< -o $@

test_import_camlp5.byte: test_import_camlp5.ml testutil.cmo testutil2.cmo stuff.cmo test_self_import.cmi
	$(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) -package $(PACKAGES),$(PACKAGEPREFIX)import.syntax,$(PACKAGEPREFIX)deriving_plugins.show.syntax \
	-ppopt -pa_import-package -ppopt $(PACKAGES) \
	-ppopt -pa_import-I -ppopt . \
	-linkall -linkpkg -syntax camlp5o testutil.cmo testutil2.cmo $< -o $@

test_self_import.cmo: test_self_import.ml test_self_import.cmi
	$(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) -package $(PACKAGES),$(PACKAGEPREFIX)import.syntax \
	-ppopt -pa_import-package -ppopt $(PACKAGES) \
	-ppopt -pa_import-I -ppopt . \
	-syntax camlp5o -c $< -o $@

test_self_import.cmi: test_self_import.mli
	$(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) -package $(PACKAGES) \
	-syntax camlp5o -c $< -o $@

test_ppx_import.byte: test_ppx_import.ml testutil.cmo testutil2.cmo stuff.cmo test_self_import.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) -package $(PACKAGES),$(PACKAGEPREFIX)import.syntax,$(PACKAGEPREFIX)deriving_plugins.show.syntax \
	-ppopt -pa_import-package -ppopt $(PACKAGES) \
	-ppopt -pa_import-I -ppopt . \
	-linkall -linkpkg -syntax camlp5o testutil.cmo testutil2.cmo stuff.cmo test_self_import.cmo $< -o $@

testutil2.cmo: testutil2.ml
	$(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) -package $(PACKAGES) -syntax camlp5o -c $<

stuff.cmo: stuff.ml
	$(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) -package $(PACKAGES) -syntax camlp5o -c $<

clean::
	rm -f *.corrected

depend::
	$(OCAMLFIND) ocamldep $(DEBUG) $(WARNERR) -package $(PACKAGES) -syntax camlp5o testutil2.ml stuff.ml test_*.ml > .depend.NEW
	$(OCAMLFIND) ocamldep $(DEBUG) $(WARNERR) -package $(PACKAGES) -syntax camlp5r testutil.ml >> .depend.NEW
	mv .depend.NEW .depend

-include .depend
