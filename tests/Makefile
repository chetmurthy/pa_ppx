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
PACKAGES := $(PACKAGES)

CMO=
CMI=$(CMO:.cmo=.cmi)

TESTS= \
	test_deriving_show.byte \
	test_deriving_eq.byte \
	test_deriving_ord.byte \
	test_deriving_enum.byte \

all: $(CMO) $(TESTS)

test_deriving_%.byte: test_deriving_%.ml
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES),pa_ppx_pa_deriving_plugins -syntax camlp5r $< -o $@

test_deriving_show.byte: test_deriving_show.ml
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES),pa_ppx_pa_deriving_plugins.show.syntax -syntax camlp5o $< -o $@
