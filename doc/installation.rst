============
Installation
============

Prerequisites
-------------

1. Ocaml

To install ``pa_ppx`` you'll need Ocaml (version >= 4.10.0) and Camlp5
(version 8.00).  You can install Ocaml in the usual way, typically via
``opam``.

2. Camlp5

To install Camlp5, for now you'll need to get `a
development version (pre-8.00)
<https://github.com/camlp5/camlp5/tree/pre-8.00>`_.

Eventually this version of Camlp5 will get released, and at that
point, you'll be able to install it with opam.  For now, you need
to build it in the usual way on the commandline
(documented over at that camlp5 repo, but here's a quick synopsis)::

  make clean && ./configure -libdir `dirname $(ocamlc -where)` && make -j32 all
  make install

3. ``ocamlfind`` and ``not-ocamlfind``

``ocamlfind`` is unavoidable, and I've written a little wrapper, `not-ocamlfind <https://github.com/chetmurthy/not-ocamlfind>`_ that provides some useful extra commands, which I'll refer to throughout this documentation::

  opam install ocamlfind not-ocamlfind

4. Various other ocaml packages::

``pa_ppx`` has tests to verify that things work with both ``pa_ppx``
and the standard PPX rewriters, so there are a ton of extra
packages needed::

  opam install rresult fmt ounit2 pcre core_kernel ppx_deriving \
       ppx_deriving_yojson ppx_import ppx_here sexplib bos \
       expect_test_helpers ppx_expect

Building and Installing
-----------------------

Once having installed camlp5, you can build and install ``pa_ppx`` via::

  make && make install

and you can run (copious) tests (some of which fail -- I'm working on that!)::

  make -k test

.. container:: trailer
