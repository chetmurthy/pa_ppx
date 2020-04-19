# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=$(WD)/..
include $(TOP)/config/Makefile

DESTDIR=
RM=rm

LAUNCH=env TOP=$(TOP) $(TOP)/tools/LAUNCH
OCAMLFIND=$(LAUNCH) ocamlfind
MKCAMLP5=$(LAUNCH) mkcamlp5
PACKAGES := $(PACKAGES),ounit2,fmt,pcre,rresult,compiler-libs.common,pa_ppx_base

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
	test_import0.byte \
	test_import_camlp5.byte \
	test_ppx_import.byte \
	test_ppx_import.ppx.byte \
	test_hashrecons.byte \
	test_passthru_hashrecons.byte \

all: $(CMO) $(TESTS) test

test: $(TESTS:.byte=)

% : %.byte
	./$<

PPXPACKAGES=ppx_import,ppx_deriving.show,ppx_deriving.eq,ppx_deriving.ord,ppx_deriving.enum,ppx_deriving.iter,ppx_deriving.map,ppx_deriving.fold

test_%.byte: test_%.ml testutil.cmo testutil2.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES),pa_ppx_pa_deriving_plugins,pa_ppx_pa_hashrecons.syntax \
	-linkall -linkpkg -syntax camlp5o testutil.cmo testutil2.cmo $< -o $@

test_%.ppx.byte: test_%.ml testutil.cmo testutil2.cmo stuff.cmo test_self_import.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES),ounit2,$(PPXPACKAGES) -linkall -linkpkg testutil.cmo testutil2.cmo stuff.cmo test_self_import.cmo $< -o $@

test_passthru_hashrecons.byte: test_passthru_hashrecons.ml testutil.cmo testutil2.cmo hCPassthru.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES),camlp5.pa_o,camlp5.pr_o,pa_ppx_base,pa_ppx_pa_hashrecons.syntax \
	-linkall -linkpkg -syntax camlp5o testutil.cmo testutil2.cmo hCPassthru.cmo $< -o $@

hCPassthru.cmo: hCPassthru.ml
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES),pa_ppx_pa_hashrecons.syntax -syntax camlp5r -c $<

test_import0.byte: test_import0.ml testutil.cmo testutil2.cmo stuff.cmo test_self_import.cmi
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES),pa_ppx_pa_import.syntax \
	-ppopt -pa_import-package -ppopt $(PACKAGES) \
	-ppopt -pa_import-I -ppopt . \
	-linkall -linkpkg -syntax camlp5o testutil.cmo testutil2.cmo $< -o $@

test_import_camlp5.byte: test_import_camlp5.ml testutil.cmo testutil2.cmo stuff.cmo test_self_import.cmi
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES),pa_ppx_pa_import.syntax,pa_ppx_pa_deriving_plugins.show.syntax \
	-ppopt -pa_import-package -ppopt $(PACKAGES) \
	-ppopt -pa_import-I -ppopt . \
	-linkall -linkpkg -syntax camlp5o testutil.cmo testutil2.cmo $< -o $@

test_self_import.cmo: test_self_import.ml test_self_import.cmi
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES),pa_ppx_pa_import.syntax \
	-ppopt -pa_import-package -ppopt $(PACKAGES) \
	-ppopt -pa_import-I -ppopt . \
	-syntax camlp5o -c $< -o $@

test_self_import.cmi: test_self_import.mli
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES) \
	-syntax camlp5o -c $< -o $@

test_ppx_import.byte: test_ppx_import.ml testutil.cmo testutil2.cmo stuff.cmo test_self_import.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES),pa_ppx_pa_import.syntax,ppx_deriving.show \
	-ppopt -pa_import-package -ppopt $(PACKAGES) \
	-ppopt -pa_import-I -ppopt . \
	-linkall -linkpkg -syntax camlp5o testutil.cmo testutil2.cmo stuff.cmo test_self_import.cmo $< -o $@

testutil2.cmo: testutil2.ml
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES) -syntax camlp5o -c $<

stuff.cmo: stuff.ml
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES) -syntax camlp5o -c $<

depend::
	$(OCAMLFIND) ocamldep $(DEBUG) -package $(PACKAGES) -syntax camlp5o testutil2.ml stuff.ml test_*.ml > .depend.NEW
	$(OCAMLFIND) ocamldep $(DEBUG) -package $(PACKAGES) -syntax camlp5r testutil.ml >> .depend.NEW
	mv .depend.NEW .depend

-include .depend
