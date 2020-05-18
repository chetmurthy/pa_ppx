
open Testutil

module NormLocations = struct
  let loc = Ploc.dummy
  let str_item = (Reloc.str_item (fun _ -> loc) 0)
  let sig_item = (Reloc.sig_item (fun _ -> loc) 0)
  let structure sil = List.map (fun (si, loc) -> (str_item si, loc)) sil
  let signature sil = List.map (fun (si, loc) -> (sig_item si, loc)) sil
end

let official_implem f =
  f |> Fpath.v |> Bos.OS.File.read
  |> Rresult.R.get_ok |> Official.Implem.pa
  |> Official.Implem.pr |> print_string

let pappx_implem f =
  f |> Fpath.v |> Bos.OS.File.read
  |> Rresult.R.get_ok |> PAPR.Implem.pa1 ~input_file:f
  |> NormLocations.structure |> List.map fst |> Ast2pt.implem f
  |> Official.Implem.pr |> print_string

let official_interf f =
  f |> Fpath.v |> Bos.OS.File.read
  |> Rresult.R.get_ok |> Official.Interf.pa
  |> Official.Interf.pr |> print_string

let pappx_interf f =
  f |> Fpath.v |> Bos.OS.File.read
  |> Rresult.R.get_ok |> PAPR.Interf.pa1 ~input_file:f
  |> NormLocations.signature |> List.map fst |> Ast2pt.interf f
  |> Official.Interf.pr |> print_string

let%expect_test "minimal implem" =
#ifdef OFFICIAL
  official_implem "testdata/minimal.ml" ;
#else
  pappx_implem "testdata/minimal.ml" ;
#endif
  [%expect {|
    [@@@ocaml.text
      " The first special comment of the file is the comment associated\n     to the whole module. "]
    let f x y = x + y[@@ocaml.doc " The comment for function f "] |}]

let%expect_test "minimal interf" =
#ifdef OFFICIAL
  official_interf "testdata/minimal.mli" ;
#else
  pappx_interf "testdata/minimal.mli" ;
#endif
  [%expect {|
    [@@@ocaml.text
      " The first special comment of the file is the comment associated\n     with the whole module."]
    [@@@ocaml.text
      " Special comments can be placed between elements and are kept\n     by the OCamldoc tool, but are not associated to any element.\n     @-tags in these comments are ignored."]
    [@@@ocaml.text
      " Comments like the one above, with more than two asterisks,\n     are ignored. "]
    val f : int -> int -> int[@@ocaml.doc " The comment for function f. "] |}]

let%expect_test "simple implem" =
#ifdef OFFICIAL
  official_implem "../pa_dock/examples/foofixed.ml" ;
#else
  pappx_implem "../pa_dock/examples/foofixed.ml" ;
#endif
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
        [@@ocaml.doc " The comment for toto "]
        val toto = 1[@@ocaml.doc " The comment for toto "]
        val titi = "titi"[@@ocaml.doc " Comment for method toto "]
        method toto = tutu ^ "!"[@@ocaml.doc " Comment for method toto "]
        [@@ocaml.doc " Comment for method m "]
        method m (f : float) = 1[@@ocaml.doc " Comment for method m "]
      end[@@ocaml.doc " The comment for class my_class "]
    class type my_class_type =
      object
        val  mutable x : int[@@ocaml.doc
                              " The comment for the instance variable x. "]
        [@@ocaml.doc " The comment for method m. "]
        method  m : int -> int[@@ocaml.doc " The comment for method m. "]
      end[@@ocaml.doc " The comment for class type my_class_type "]
    module Foo =
      struct
        let x = 0[@@ocaml.doc " The comment for x "]
        [@@@ocaml.text " A special comment in the class, but not associated to any element. "]
      end[@@ocaml.doc " The comment for module Foo "]
    module type my_module_type  = sig val x : int[@@ocaml.doc " Comment for value x. "] end
    [@@ocaml.doc " The comment for module type my_module_type. "]
    [@@@ocaml.text " a special comment at the end of the toplevel module "] |}]

let%expect_test "simple interf" =
#ifdef OFFICIAL
  official_interf "../pa_dock/examples/foo.mli" ;
#else
  pappx_interf "../pa_dock/examples/foo.mli" ;
#endif
  [%expect {| |}]
