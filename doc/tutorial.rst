========
Tutorial
========

Using PPX Rewriters from Pa_ppx
-------------------------------

To use PPX Rewriters from Pa_ppx, let's start with a really simple file that works with the standard PPX rewriters (simple_show.ml)::

  type a1 = int * int [@@deriving show]
  let _ = print_string ([%show: a1] (5,6))

and we compile it thus::

  ocamlfind ocamlc -package ppx_deriving.show simple_show.ml  -o simple_show.byte

Running it yields::

  $ ./simple_show.byte
  (5, 6)

To compile with ``pa_ppx``::

  ocamlfind ocamlc -package pa_ppx.runtime,pa_ppx.deriving_plugins.show -syntax camlp5o simple_show.ml -linkall -linkpkg  -o simple_show.byte

with identical output::

  $ ./simple_show.byte
  (5, 6)

There's really only two important differences:

1. need to specify the syntax (``-syntax camlp5o``)
2. need to specify the runtime support module ``pa_ppx.runtime``

The other linking flags, I just haven't figured out precisely how to get rid of.

Sometimes more packages must be specified (e.g. for ``expect_test``
and ``inline_test``) because ``dune`` is adding those
under-the-covers, and these instructions are all Makefile-friendly.

Writing new PPX Rewriters upon Pa_ppx
-------------------------------------

NOTE WELL: All code in this section is written in "revised" syntax.
Much of this will work in "official" syntax, but since Camlp5 itself
is written in revised syntax, I kept on going that way.

In this section, we will describe the simplest rewriter
(``pa_ppx.here``).  This rewriter replaces the extension point
``[%here]`` with code that produces a ``Lexing.position`` of the
position in the file where the extension-point was found.  So a line (in a file "test_here.ml")::

  vslue here = [%here] ;

is rewritten to::

  value here =
    let open Lexing in
    {pos_fname = "test_here.ml"; pos_lnum = 4; pos_bol = 32;
     pos_cnum = 43}
  ;

We won't go into excruciating detail, because this depends on a number
of ``camlp5`` and ``pa_ppx`` facilities that are described in more
detail either in the ``camlp5`` documentation, or elsewhere in this
documentation.

1. Open necessary libraries (``Pa_ppx_base`` contains support
infrastructure for all PPX rewriters)::

  open Pa_ppx_base ;
  open Pa_passthru ;
  open Ppxutil ;

2. Implement a function that rewrites the simple extension-point,
using ``camlp5`` "quotations".  The function ``quote_position`` uses
quotations for expressions, with anti-quotations ("holes") for
expressinos we want to fill with bits from the ``Lexing.position``::

  value quote_position loc p =
    let open Lexing in
    <:expr< let open Lexing in {
      pos_fname = $str:p.pos_fname$ ;
      pos_lnum = $int:string_of_int p.pos_lnum$ ;
      pos_bol = $int:string_of_int p.pos_bol$ ;
      pos_cnum = $int:string_of_int p.pos_cnum$ } >>
  ;

Next we write a function that pattern-matches on an expression
(expected to be ``[%here]``) and rewrites it using ``quote_position``::

  value rewrite_expr arg = fun [
    <:expr:< [%here] >> ->
      let pos = start_position_of_loc loc in
      quote_position loc pos
  | _ -> assert False
  ]
  ;

And finally, we add this function to the "extensible function" for
expressions.  The type ``EF.t`` is a dispatch table of "extension
points", one for each important type in the Camlp5 ML AST.  All these
extension-points start off empty, and we want to add our function to
the extension-point for expressions.  Then we "install" this table in
the ``Pa_passthru`` module, giving it a name.  We can specify that it
comes before or after other rewriters, or specify a pass number
(0..99), though this is almost never used.  Instead, by specifying
which rewriters to run before or after, we give ``Pa_passthru`` the
information to topologically sort all loaded rewriters before running
them::

  value install () = 
  let ef = EF.mk () in 
  let ef = EF.{ (ef) with
            expr = extfun ef.expr with [
    <:expr:< [%here] >> as z ->
    fun arg ->
      Some (rewrite_expr arg z)
  ] } in
  Pa_passthru.(install { name = "pa_here"; ef =  ef ; pass = None ; before = [] ; after = [] })
  ;
  install();

An example of a rewriter that specifies a "before" constraint would be
``pa_ppx.import``, which should be run before ``pa_ppx.deriving``, so
that a type can be imported, and then have type-based code derived
from that imported type.

.. container:: trailer
