Implementation of PPX rewriters using camlp5 infrastructure.

# Introduction

This is a re-implementation of a number of the PPX rewriters from
ocaml-ppx, but based on camlp5 infrastructure instead of ppxlib.

So: right up front: if you're writing PPX rewriters based on ppxlib,
ppx_deriving, etc, then they *will not work with this package*.  Not.
At.  All.  Instead, the goal here is to show how easy it is to write
PPX rewriters this way.

Also, this code is barely two weeks old, so you should expect bugs,
and I'm happy to fix them posthaste, b/c I want this code to work
well.

This code (re-)implements:

1. all of ppx_deriving: pa_ppx.deriving_plugins.{enum,eq,fold,iter,make,map,ord,show}
2. ppx_import: pa_import
3. [for internal use by PPX rewriters using this infrastructure] pa_unmatched_vala
4. [an example] pa_hashrecons
5. ppx_deriving_yojson: pa_ppx.deriving_plugins.yojson (partially broken)

the re-implementations of ppx_deriving and ppx_import pass all their
unit-tests and use the same syntax of attributes as the original
rewriters.  **You should be able to use your code with these packages
unmodified; anything else is a bug with these packages**.

# Installation

This code depends on ocaml 4.10.0 and a pre-release version of camlp5.

Ocaml: version 4.10.0 -- install using "opam create_switch 4.10.0"

Camlp5 pre-8.00: [Camlp5](https://github.com/chetmurthy/camlp5)

Eventually this will get released, and at that point, you'll be able
to install it with opam.

For now, you need to build it in the usual way on the commandline (documented over at that camlp5 repo, but here's a quick synopsis):
```
make clean && ./configure -libdir `dirname $(ocamlc -where)` && make -j32 all && make -C testsuite/ clean all-tests

make install
```

pa_ppx (this package):

```
make && make install
```

and you can run tests (the Yojson test fails to build):
```
make -C tests -k all
```

# Organization of Findlib packages

There are a bunch of findlib packages.  Maybe too many and too
confusing, I can't tell.  But the general idea is that for each
rewriter or group of rewriters, there are two packages:

1. the package for loading into the toplevel or linking into a commandline tool, viz. `pa_ppx.deriving.plugins.show`
2. the package for adding to camlp5 during preprocessing, viz. `pa_ppx.deriving.plugins.show.syntax`

Note the ".syntax" at the end there.  These are separated like this so
we can specify "preprocess with the show plugin, but don't link it
into the program" and separately "link the show plugin into the
program, but don't preprocess with it".

[I thought of having a "virtual package" that just required the both,
but it turns out that there's a bug in findlib that prevents this from
working correctly in the context of preprocessing.  So I'm stuck with
this for now.]

# Using with Makefiles

There are copious examples in the `tests` directory: these are copied
over almost-verbatim from ppx_deriving, and show almost all the
functionality being exercised.  Here is a simple one, stripped-down to
the minimum:
```
ocamlfind ocamlc -verbose -package rresult,ounit2,pa_ppx.runtime,pa_ppx.deriving_plugins.std.syntax \
	-syntax camlp5o -linkpkg -linkall test_deriving_show.ml -o test_deriving_show.byte
```

1. specify syntax `-syntax camlp5o`
2. specify packages needed by the test itself: `rresult,ounit2`
3. specify pa_ppx runtime support package: `pa_ppx.runtime`
4. specify the rewriter packages: `pa_ppx.deriving_plugins.std.syntax`

# Using with Dune

Dune requires that we provide a command that will preprocess, but not
compile.  Since this is cumbersome to do with ocamlfind, `pa_ppx`
builds such a preprocessor and installs it as part of the package,
linked with all the rewriting plugins.  This can be invoked this:
```
ocamlfind pa_ppx/camlp5o.pa_ppx  ./test_deriving_show.ml
```
[BTW, this command was built using `mkcamlp5`, and you can see the build command in the `pa_ppx` top-level Makefile.]

With this command, we can modify a dune file pretty easily.  Here's the modification for [yara-ocaml](https://github.com/XVilka/yara-ocaml):
```
@@ -2,7 +2,13 @@
  (name yara)
  (public_name yara)
  (wrapped false)
- (preprocess
-  (pps ppx_deriving.std))
+
+;; (preprocess
+;;  (pps ppx_deriving.std))
+
+ (preprocess (action
+      (run ocamlfind pa_ppx/camlp5o.pa_ppx %{input-file})
+    ))
+
```

and here's a dunefile that will compile `test_deriving_show.ml`:
```
(env
  (dev
    (flags (:standard -w -27 -w -32))))

(test
 (name test_deriving_show)
 (libraries oUnit fmt ppx_deriving.runtime)
 (preprocess (action
      (run ocamlfind pa_ppx/camlp5o.pa_ppx %{input-file})
    )))
```

[The warnings must be silenced b/c the test itself elicits warnings,
and I didn't want to modify it.  OTOH, I didn't silence all warnings
b/c if there are warning produced by the generated code, I'd like to
know about them.]
