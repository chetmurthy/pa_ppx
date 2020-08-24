# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile

DESTDIR=
RM=rm

LAUNCH=env TOP=$(TOP) $(TOP)/tools/LAUNCH
OCAMLFIND=$(LAUNCH) ocamlfind
NOT_OCAMLFIND=$(LAUNCH) not-ocamlfind

PACKAGES = rresult,fmt,camlp5,yojson,sexplib
TARGET = pa_ppx_runtime.cma
ML = runtime.ml exceptions.ml
MLI = exceptions.mli
CMO = $(ML:.ml=.cmo)
CMI = $(ML:.ml=.cmi)
CMX = $(ML:.ml=.cmx)
CMT = $(ML:.ml=.cmt)
CMTI = $(MLI:.mli=.cmti)

OCAMLCFLAGS := $(OCAMLCFLAGS) -for-pack Pa_ppx_runtime -linkall

all: $(TARGET) $(TARGET:.cma=.cmxa) local-install

doc: $(CMT) $(CMTI)

pa_ppx_runtime.cmo: $(CMO)
	$(OCAMLFIND) ocamlc $(DEBUG) -pack -o pa_ppx_runtime.cmo $^

pa_ppx_runtime.cmx: $(CMO:.cmo=.cmx)
	$(OCAMLFIND) ocamlopt $(DEBUG) -pack -o pa_ppx_runtime.cmx $^

META: META.pl
	./META.pl > META
install::
	mkdir -p $(DESTDIR)/lib
	./META.pl $(DESTDIR)/lib > META
	$(NOT_OCAMLFIND) reinstall-if-diff pa_ppx_runtime -destdir $(DESTDIR)/lib META $(TARGET) $(TARGET:.cma=.cmxa) $(TARGET:.cma=.a) pa_ppx_runtime.cmx pa_ppx_runtime.cmi $(wildcard *.cmt*)
	$(RM) -f META

local-install::
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

clean::
	rm -rf META camlp5.pa_ppx_runtime

$(TARGET): pa_ppx_runtime.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) pa_ppx_runtime.cmo -a -o $(TARGET)

$(TARGET:.cma=.cmxa): pa_ppx_runtime.cmx
	$(OCAMLFIND) ocamlopt $(DEBUG) pa_ppx_runtime.cmx -a -o $(TARGET:.cma=.cmxa)

$(TARGET): $(CMO)
$(TARGET:.cma=.cmxa): $(CMO:.cmo=.cmx)

initialize-exn-i:: exceptions.ORIG.mli
	$(NOT_OCAMLFIND) preprocess $(DEBUG) -package $(PACKAGES),camlp5.macro,camlp5.pr_r -syntax camlp5o $< > $@.NEW
	mv $@.NEW exceptions.mli

bootstrap-exn-i: exceptions.ORIG.mli
	$(NOT_OCAMLFIND) preprocess $(DEBUG) -package $(PACKAGES),camlp5.macro,camlp5.pr_r,pa_ppx_import,pa_ppx_deriving_plugins.std,pa_ppx_deriving_plugins.yojson,pa_ppx_deriving_plugins.sexp \
	-syntax camlp5o \
	-ppopt -pa_ppx-runtime -ppopt "" \
	-ppopt -D -ppopt BOOTSTRAP \
	-ppopt -pa_import-package -ppopt $(PACKAGES) \
	-ppopt -pa_import-I -ppopt . \
	-ppopt -sep -ppopt "\n" \
	$< > $@.NEW
	mv $@.NEW  exceptions.mli

refold-exn-i: exceptions.mli
	$(NOT_OCAMLFIND) preprocess $(DEBUG) -package $(PACKAGES),camlp5.pr_r,pa_ppx_undo_deriving \
	-syntax camlp5r \
	$< > $@.NEW
	mv $@.NEW  exceptions.REFOLD.mli

initialize-exn:: exceptions.ORIG.ml
	$(NOT_OCAMLFIND) preprocess $(DEBUG) -package $(PACKAGES),camlp5.macro,camlp5.pr_r -syntax camlp5o $< > $@.NEW
	mv $@.NEW exceptions.ml

bootstrap-exn: exceptions.ORIG.ml
	$(NOT_OCAMLFIND) preprocess $(DEBUG) -package $(PACKAGES),camlp5.macro,camlp5.pr_r,pa_ppx_import,pa_ppx_deriving_plugins.show \
	-syntax camlp5o \
	-ppopt -pa_ppx-runtime -ppopt "" \
	-ppopt -D -ppopt BOOTSTRAP \
	-ppopt -pa_import-package -ppopt $(PACKAGES) \
	-ppopt -pa_import-I -ppopt . \
	-ppopt -sep -ppopt "\n" \
	$< > $@.NEW
	mv $@.NEW  exceptions.ml

refold-exn: exceptions.ml
	$(NOT_OCAMLFIND) preprocess $(DEBUG) -package $(PACKAGES),camlp5.pr_r,pa_ppx_undo_deriving \
	-syntax camlp5r \
	$< > $@.NEW
	mv $@.NEW  exceptions.REFOLD.ml

EXTERNAL = $(shell $(OCAMLFIND) query -predicates byte -format '%m' $(PACKAGES))
$(CMO) $(CMI) $(CMX): $(EXTERNAL)

depend::
	echo "$(CMO) $(CMI) $(CMX): $(EXTERNAL)" > .depend.NEW
	$(OCAMLFIND) ocamldep $(DEBUG) -package $(PACKAGES) -syntax camlp5r runtime.ml exceptions.ml exceptions.mli >> .depend.NEW \
		&& mv .depend.NEW .depend

-include .depend
