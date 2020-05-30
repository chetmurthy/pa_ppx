============
Installation
============

To install ``pa_ppx`` you'll need Ocaml (version >= 4.10.0) and Camlp5
(version 8.00).  You can install Ocaml in the usual way, typically via
``opam``.  To install Camlp5, for now you'll need to get `a
development version (pre-8.00)
<https://github.com/camlp5/camlp5/tree/pre-8.00>`_.

Eventually this version of Camlp5 will get released, and at that
point, you'll be able to install it with opam.  For now, you need
to build it in the usual way on the commandline
(documented over at that camlp5 repo, but here's a quick synopsis)::

  make clean && ./configure -libdir `dirname $(ocamlc -where)` && make -j32 all
  make install

Once having installed camlp5, you can build and install ``pa_ppx`` via::

  make && make install

and you can run (copious) tests (some of which fail -- I'm working on that!)::

  make -k test

.. container:: trailer
