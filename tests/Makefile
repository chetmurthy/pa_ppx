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
	test_import0.byte \
	test_import0.ppx.byte \
	test_ppx_import.byte \
	test_ppx_import.ppx.byte \

all: $(CMO) $(TESTS) test

test: $(TESTS:.byte=)

% : %.byte
	./$<

test_%.byte: test_%.ml testutil.cmo testutil2.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES),pa_ppx_pa_deriving_plugins \
	-linkall -linkpkg -syntax camlp5o testutil.cmo testutil2.cmo $< -o $@

test_%.ppx.byte: test_%.ml testutil.cmo testutil2.cmo stuff.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES),ounit2,ppx_deriving.show,ppx_deriving.eq,ppx_deriving.ord,ppx_deriving.enum,ppx_import -linkall -linkpkg testutil.cmo testutil2.cmo $< -o $@

test_import0.byte: test_import0.ml testutil.cmo testutil2.cmo stuff.cmo test_self_import.cmi
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES),pa_ppx_pa_import \
	-ppopt -pa_import-package -ppopt $(PACKAGES) \
	-ppopt -pa_import-I -ppopt . \
	-linkall -linkpkg -syntax camlp5o testutil.cmo testutil2.cmo $< -o $@

test_self_import.cmo: test_self_import.ml test_self_import.cmi
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES),pa_ppx_pa_import \
	-ppopt -pa_import-package -ppopt $(PACKAGES) \
	-ppopt -pa_import-I -ppopt . \
	-syntax camlp5o -c $< -o $@

test_self_import.cmi: test_self_import.mli
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES) \
	-syntax camlp5o -c $< -o $@

test_ppx_import.byte: test_ppx_import.ml testutil.cmo testutil2.cmo stuff.cmo test_self_import.cmi
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES),pa_ppx_pa_import \
	-ppopt -pa_import-package -ppopt $(PACKAGES) \
	-ppopt -pa_import-I -ppopt . \
	-linkall -linkpkg -syntax camlp5o testutil.cmo testutil2.cmo $< -o $@

testutil2.cmo: testutil2.ml
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES) -syntax camlp5o -c $<

stuff.cmo: stuff.ml
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES) -syntax camlp5o -c $<

testutil2.cmo: testutil.cmo
