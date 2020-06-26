====================
Pa_ppx Documentation
====================

This is the documentation for the `pa_ppx
<https://github.com/chetmurthy/pa_ppx>`_ project, a re-implementation
of many "standard" PPX rewriters, but using `Camlp5
<https://github.com/camlp5/camlp5>`_ infrastructure.  

You can learn more about ``pa_ppx`` at :ref:`introduction` but
quickly, ``pa_ppx`` provides implementations of

1. all of ``ppx_deriving``: ``pa_ppx.deriving_plugins.{enum,eq,fold,iter,make,map,ord,show}``
2. ``ppx_import``: ``pa_ppx.import``
3. ``ppx_deriving_yojson``: ``pa_ppx.deriving_plugins.yojson``
4. ``ppx_sexp_conv``: ``pa_ppx.deriving_plugins.sexp``
5. ``ppx_inline_test``: ``pa_ppx.inline_test``
6. ``ppx_expect_test``: ``pa_ppx.expect_test``
7. ``ppx_assert``: ``pa_ppx.assert``
8. ``ppx_here``: ``pa_ppx.here``
9. ``ppx_deriving_protobuf``: ``pa_ppx.deriving_plugins.protobuf``

and a few other experimental PPX rewriters.  For many of the
re-implementations, there are extensions beyond what the original
implementation provided (e.g. support everywhere possible, for
extensible variants and exceptions, better type-import suppor).  All
of this is built on an infrastructure for writing PPX rewriters, based
on Camlp5.

Rationale for ``pa_ppx``
========================

One can think of this project as an experiment to prove the following
thesis:

    To build an effective and usable macro-preprocessor system for a
    language with the rich and complex AST structure of Ocaml, one
    needs to provide facilities for manipulating the AST using the
    surface syntax, not merely the syntax of data-types in the
    language; this requires both *patterns* and *expressions* freely
    available in code (for matching and constructing AST fragments)
    with placeholders (anti-quotations) for binding sub-trees and
    introducing computed fragments.

    A flexible and *transparent* mechanism for introducing rewriting
    code into the system is also necessary.

    In this project, we're trying to prove this thesis by using
    Camlp5: specifically its system of **quotations** and
    **antiquotations** for *patterns* and *expressions*, as well as
    **extensible functions** as the chief mechanism for introducing
    new rewriters into the system.

    The *transparency* and *comprehensibility* of the resulting
    examples, is the point of the exercise.
  

.. toctree::
   :maxdepth: 3

   intro
   installation
   tutorial
   builtins
   build-systems
   faq
