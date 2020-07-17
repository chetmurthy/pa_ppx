Implementation of PPX rewriters using camlp5 infrastructure.

# Overview

This is a re-implementation of a number of the PPX rewriters from
ocaml-ppx, but based on camlp5 infrastructure instead of ppxlib.

So: right up front: if you're writing PPX rewriters based on ppxlib,
ppx_deriving, etc, then they *will not work with this package*.  Not.
At.  All.  Instead, the goal here is to show how easy it is to write
PPX rewriters this way.

Also, this code is barely two months old, so you should expect bugs,
and I'm happy to fix them posthaste, b/c I want this code to work
well.

All the documentation for ``pa_ppx`` is in Sphinx/RST format, and can
be found [in this distribution](./doc/_build/index.html) and also [at
readthedocs](https://pa-ppx.readthedocs.io/en/latest/index.html).
There you will find instructions on installation, using with Makefiles
and Dune, a tutorial walkthru of using ``pa_ppx``, for writing new PPX
rewriters on this infrastructure, and some documentation on each
rewriter (this last bit is definitely work-in-progress).

Quickly, this code (re-)implements:

1. all of ppx_deriving
2. ppx_import
3. ppx_deriving_yojson
4. ppx_sexp_conv
5. ppx_inline_test
6. ppx_expect_test
7. ppx_assert
8. ppx_here

It also implements some new rewriters:

9. pa_ppx.dock: doc-comment extractor
10. [for internal use by PPX rewriters using this infrastructure] pa_unmatched_vala
11. [an example] pa_hashrecons
12. pa_ppx.undo_deriving: undoes the effect of pa_ppx.deriving and its plugins

the re-implementations of ppx_deriving and ppx_import pass all their
unit-tests and use the same syntax of attributes as the original
rewriters.  **You should be able to use your code with these packages
unmodified; anything else is a bug with these packages**.

# Documentation

All the documentation for ``pa_ppx`` is in Sphinx/RST format, and can
be found [in this distribution](./doc/_build/index.html) and also [at
readthedocs](https://pa-ppx.readthedocs.io/en/latest/index.html).
