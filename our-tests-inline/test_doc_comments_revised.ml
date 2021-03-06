
open Pa_ppx_testutils
open Testutil
open Dock_test_utils

let%expect_test "minimal implem" =
  pappx_implem_string
 {|(** The first special comment of the file is the comment associated
     to the whole module. *)

 (** The comment for function f *)
 value f x y = x + y ;
|} ;
  [%expect {|
    [@@@ocaml.text
      " The first special comment of the file is the comment associated\n     to the whole module. "]
    let f x y = x + y[@@ocaml.doc " The comment for function f "] |}]

let%expect_test "minimal interf" =
  pappx_interf_string
{|(** The first special comment of the file is the comment associated
     with the whole module.*)


 (** Special comments can be placed between elements and are kept
     by the OCamldoc tool, but are not associated to any element.
     @-tags in these comments are ignored.*)

 (*******************************************************************)
 (** Comments like the one above, with more than two asterisks,
     are ignored. *)

 (** The comment for function f. *)
 value f : int -> int -> int ;
|} ;
  [%expect {|
    [@@@ocaml.text
      " The first special comment of the file is the comment associated\n     with the whole module."]
    [@@@ocaml.text
      " Special comments can be placed between elements and are kept\n     by the OCamldoc tool, but are not associated to any element.\n     @-tags in these comments are ignored."]
    [@@@ocaml.text
      " Comments like the one above, with more than two asterisks,\n     are ignored. "]
    val f : int -> int -> int[@@ocaml.doc " The comment for function f. "] |}]


let%expect_test "interface leading comment" =
  pappx_interf_string
{|(** The first special comment of the file is the comment associated
     with the whole module.*)

 value f : int -> int -> int ;
|} ;
  [%expect {|
    [@@@ocaml.text
      " The first special comment of the file is the comment associated\n     with the whole module."]
    val f : int -> int -> int |}]

let%expect_test "doc-before-value" =
  pappx_implem_string
 {|
 (** 1 *)
 value f x y = x + y ;

 (** 2 *)
|} ;
  [%expect {|
    let f x y = x + y[@@ocaml.doc " 1 "]
    [@@@ocaml.text " 2 "]
 |}]

let%expect_test "implem: variants" =
  pappx_implem_string
 {|
(** 0 *)
  type t = [ A (** 1 *) | B (** 2 *) | C (** 3 *) ] ;
|} ;
  [%expect {|
    type t =
      | A [@ocaml.doc " 1 "]
      | B [@ocaml.doc " 2 "]
      | C [@ocaml.doc " 3 "][@@ocaml.doc " 0 "]
 |}]

let%expect_test "implem: struct+variants" =
  pappx_implem_string
 {|
module M = struct
  type t = [ A (** 1 *) | B (** 2 *) | C (** 3 *) ] ;
end ;
|} ;
  [%expect {|
module M =
  struct
    type t =
      | A [@ocaml.doc " 1 "]
      | B [@ocaml.doc " 2 "]
      | C [@ocaml.doc " 3 "]
  end
 |}]

let%expect_test "interf: sig+variants" =
  pappx_interf_string
 {|
module type S = sig
(** 0 *)
  type t = [ A (** 1 *) | B (** 2 *) | C (** 3 *) ] ;
(** 4 *)
end ;
|} ;
  [%expect {|
module type S  =
  sig
    type t =
      | A [@ocaml.doc " 1 "]
      | B [@ocaml.doc " 2 "]
      | C [@ocaml.doc " 3 "][@@ocaml.doc " 0 "][@@ocaml.doc " 4 "]
  end
 |}]

let%expect_test "interf: variants" =
  pappx_interf_string
 {|
(** 0 *)
  type t = [ A (** 1 *) | B (** 2 *) | C (** 3 *) ] ;
(** 4 *)
|} ;
  [%expect {|
  type t =
    | A [@ocaml.doc " 1 "]
    | B [@ocaml.doc " 2 "]
    | C [@ocaml.doc " 3 "][@@ocaml.doc " 0 "][@@ocaml.doc " 4 "]
 |}]

let%expect_test "implem: class+method" =
  pappx_implem_string
 {|
 class type my_class_type =
   object
     (** The comment for the instance variable x. *)
     value mutable x : int ;

     (** The comment for method m. *)
     method m : int -> int ;
   end ;
|} ;
  [%expect {|
    class type my_class_type =
      object
        val  mutable x : int[@@ocaml.doc
                              " The comment for the instance variable x. "]
        method  m : int -> int[@@ocaml.doc " The comment for method m. "]
      end
 |}]

let%expect_test "interf: record" =
  pappx_interf_string
 {|
 (** The comment for type my_record *)
 type my_record = {
     foo : int ;    (** Comment for field foo *)
     bar : string   (** Comment for field bar *)
   } ;
   (** Continuation of comment for type my_record *)
|} ;
  [%expect {|
    type my_record =
      {
      foo: int [@ocaml.doc " Comment for field foo "];
      bar: string [@ocaml.doc " Comment for field bar "]}[@@ocaml.doc
                                                           " The comment for type my_record "]
    [@@ocaml.doc " Continuation of comment for type my_record "]
 |}]

let%expect_test "interf: exception" =
  pappx_interf_string
 {|

 (** Comment for exception My_exception, even with a simple comment
     between the special comment and the exception.*)
 (* Hello, I'm a simple comment :-) *)
 exception My_exception of (int -> int) and int ;

|} ;
  [%expect {|
    exception My_exception of (int -> int) * int
      [@ocaml.doc
        " Comment for exception My_exception, even with a simple comment\n     between the special comment and the exception."]
 |}]
