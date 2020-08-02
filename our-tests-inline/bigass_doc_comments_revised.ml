
open Pa_ppx_testutils
open Testutil
open Dock_test_utils

let%expect_test "simple implem" =
  pappx_implem_file "../pa_dock/examples/foofixed_revised.ml" ;
  [%expect {|
    [@@@ocaml.text
      " The first special comment of the file is the comment associated\n     to the whole module. "]
    let f x y = x + y[@@ocaml.doc " The comment for function f "]
    [@@@ocaml.text
      " This comment is not attached to any element since there is another\n     special comment just before the next element. "]
    exception My_exception of (int -> int) * int
      [@ocaml.doc
        " Comment for exception My_exception, even with a simple comment\n     between the special comment and the exception."]
    type weather =
      | Rain of int [@ocaml.doc " The comment for constructor Rain "]
      | Sun [@ocaml.doc " The comment for constructor Sun "][@@ocaml.doc
                                                              " Comment for type weather  "]
    type my_record =
      {
      foo: int [@ocaml.doc " Comment for field foo "];
      bar: string [@ocaml.doc " Comment for field bar "]}[@@ocaml.doc
                                                           " The comment for type my_record "]
    class cl = object  end
    class my_class =
      object
        inherit  cl[@@ocaml.doc " A comment to describe inheritance from cl "]
        val mutable tutu = "tutu"[@@ocaml.doc
                                   " The comment for the instance variable tutu "]
        val toto = 1[@@ocaml.doc " The comment for toto "]
        val titi = "titi"
        method toto = tutu ^ "!"[@@ocaml.doc " Comment for method toto "]
        method m (f : float) = 1[@@ocaml.doc " Comment for method m "]
      end[@@ocaml.doc " The comment for class my_class "]
    class type my_class_type =
      object
        val  mutable x : int[@@ocaml.doc
                              " The comment for the instance variable x. "]
        method  m : int -> int[@@ocaml.doc " The comment for method m. "]
      end[@@ocaml.doc " The comment for class type my_class_type "]
    module Foo =
      struct
        let x = 0[@@ocaml.doc " The comment for x "]
        [@@@ocaml.text
          " A special comment in the class, but not associated to any element. "]
      end[@@ocaml.doc " The comment for module Foo "]
    module type my_module_type  =
      sig val x : int[@@ocaml.doc " Comment for value x. "] end[@@ocaml.doc
                                                                 " The comment for module type my_module_type. "]
    [@@@ocaml.text " a special comment at the end of the toplevel module "] |}]

let%expect_test "simple interf" =
  pappx_interf_file "../pa_dock/examples/foo_revised.mli" ;
  [%expect {|
    [@@@ocaml.text
      " The first special comment of the file is the comment associated\n     with the whole module."]
    [@@@ocaml.text
      " Special comments can be placed between elements and are kept\n     by the OCamldoc tool, but are not associated to any element.\n     @-tags in these comments are ignored."]
    [@@@ocaml.text
      " Comments like the one above, with more than two asterisks,\n     are ignored. "]
    val f : int -> int -> int[@@ocaml.doc " The comment for function f. "]
    [@@ocaml.doc " The continuation of the comment for function f. "]
    exception My_exception of (int -> int) * int
      [@ocaml.doc
        " Comment for exception My_exception, even with a simple comment\n     between the special comment and the exception."]
    type weather =
      | Rain of int [@ocaml.doc " The comment for constructor Rain "]
      | Sun [@ocaml.doc " The comment for constructor Sun "][@@ocaml.doc
                                                              " Comment for type weather  "]
    type weather2 =
      | Rain of int [@ocaml.doc " The comment for constructor Rain "]
      | Sun [@ocaml.doc " The comment for constructor Sun "][@@ocaml.doc
                                                              " Comment for type weather2  "]
    [@@ocaml.doc
      " I can continue the comment for type weather2 here\n   because there is already a comment associated to the last constructor."]
    type my_record =
      {
      foo: int [@ocaml.doc " Comment for field foo "];
      bar: string [@ocaml.doc " Comment for field bar "]}[@@ocaml.doc
                                                           " The comment for type my_record "]
    [@@ocaml.doc " Continuation of comment for type my_record "]
    val foo : string[@@ocaml.doc " Comment for foo "][@@ocaml.doc
                                                       " This comment is associated to foo and not to bar. "]
    val bar : string[@@ocaml.doc " This comment is associated to bar. "]
    class cl : object  end
    class my_class :
      object
        inherit cl[@@ocaml.doc " A comment to describe inheritance from cl "]
        val  mutable tutu : string[@@ocaml.doc
                                    " The comment for attribute tutu "]
        val  toto : int[@@ocaml.doc " The comment for attribute toto. "]
        [@@@ocaml.text
          " This comment is not attached to titi since\n         there is a blank line before titi, but is kept\n         as a comment in the class. "]
        val  titi : string
        method  toto : string[@@ocaml.doc " Comment for method toto "]
        method  m : float -> int[@@ocaml.doc " Comment for method m "]
      end[@@ocaml.doc " The comment for class my_class "]
    class type my_class_type =
      object
        val  mutable x : int[@@ocaml.doc " The comment for variable x. "]
        method  m : int -> int[@@ocaml.doc " The comment for method m. "]
      end[@@ocaml.doc " The comment for the class type my_class_type "]
    module Foo :
    sig
      val x : int[@@ocaml.doc " The comment for x "]
      [@@@ocaml.text
        " A special comment that is kept but not associated to any element "]
    end[@@ocaml.doc " The comment for module Foo "]
    module type my_module_type  =
      sig
        val x : int[@@ocaml.doc " The comment for value x. "]
        module M : sig val y : int[@@ocaml.doc " The comment for value y. "] end
        [@@ocaml.doc " The comment for module M. "]
      end[@@ocaml.doc " The comment for module type my_module_type. "] |}]
