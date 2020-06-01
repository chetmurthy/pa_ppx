====================
Pa_ppx Documentation
====================

This is the documentation for the `pa_ppx
<https://github.com/chetmurthy/pa_ppx>`_ project, a re-implementation
of many "standard" PPX rewriters, but using `Camlp5
<https://github.com/camlp5/camlp5>`_ infrastructure.  One can think of
this project as an experiment to prove the following thesis:

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
