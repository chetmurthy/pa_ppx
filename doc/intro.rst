.. _introduction:

============
Introduction
============

PPX Rewriters were introduced in Ocaml as a way of allowing
macro-preprocessing of Ocaml without also requiring extending the
parser, as in `Camlp4/5 <http://camlp5.github.io>`_.  You can `Learn
more about PPX <http://ocamllabs.io/doc/ppx.html>`_, but here we'll
assume you know how they work.  To refresh the memory, PPX consists in
*annotations* and *extensions* in the parse-tree, viz.::

  type a2 = int32 [@@deriving show] (* this is an annotation *)
  let x = [%getenv "FOO"]           (* this is an extension *)

and *preprocessors* (called "PPX rewriters") that pass over the
parse-tree (parsed by the standard Ocaml parser, with standard syntax)
and *rewrite* those annotations and extensions to plain Ocaml code.
There are several support libraries for writing PPX rewriters (`ppxlib
<https://github.com/ocaml-ppx/ppxlib>`_, `ppx_deriving
<https://github.com/ocaml-ppx/ppx_deriving>`_, probably others).

``Pa_ppx`` consists in infrastructure to allow writing those PPX
rewriters using Camlp5 as a base, and hence offers a competing
architecture for writing PPX rewriters.  To prove this point,
``Pa_ppx`` includes re-implementations of many PPX rewriters:

1. all of ``ppx_deriving``: ``pa_ppx.deriving_plugins.{enum,eq,fold,iter,make,map,ord,show}``
2. ``ppx_import``: ``pa_ppx.import``
3. ``ppx_deriving_yojson``: ``pa_ppx.deriving_plugins.yojson``
4. ``ppx_sexp_conv``: ``pa_ppx.deriving_plugins.sexp``
5. ``ppx_inline_test``: ``pa_ppx.inline_test``
6. ``ppx_expect_test``: ``pa_ppx.expect_test``
7. ``ppx_assert``: ``pa_ppx.assert``
8. ``ppx_here``: ``pa_ppx.here``
9. ``ppx_deriving_protobuf``: ``pa_ppx.deriving_plugins.protobuf``

Several of these have improvements over the ones based on ``ppxlib``
(better support for extensible variants and exceptions, better
type-import support).

In this documentation, I'll try to document how to *use* these PPX
rewriters, and how to *write* new ones with this infrastructure.

All of this requires a new version of Camlp5, as documented in the `Installation`_ section.

.. container:: trailer
