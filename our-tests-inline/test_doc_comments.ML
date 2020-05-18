
open Testutil

module NormLocations = struct
  let loc = Ploc.dummy
  let str_item = (Reloc.str_item (fun _ -> loc) 0)
  let sig_item = (Reloc.sig_item (fun _ -> loc) 0)
  let structure sil = List.map (fun (si, loc) -> (str_item si, loc)) sil
  let signature sil = List.map (fun (si, loc) -> (sig_item si, loc)) sil
end

let official_implem_file f =
  f |> Fpath.v |> Bos.OS.File.read
  |> Rresult.R.get_ok |> Official.Implem.pa
  |> Official.Implem.pr |> print_string

let pappx_implem_file f =
  f |> Fpath.v |> Bos.OS.File.read
  |> Rresult.R.get_ok |> PAPR.Implem.pa1 ~input_file:f
  |> NormLocations.structure |> List.map fst |> Ast2pt.implem f
  |> Official.Implem.pr |> print_string

let official_implem_string s =
  s |> Official.Implem.pa
  |> Official.Implem.pr |> print_string

let pappx_implem_string s =
  let f = "file://stashed" in
  Pa_ppx_dock.Pa_dock.stashed_file_contents := s ;
  s |> PAPR.Implem.pa1 ~input_file:f
  |> NormLocations.structure |> List.map fst |> Ast2pt.implem f
  |> Official.Implem.pr |> print_string

let official_interf_file f =
  f |> Fpath.v |> Bos.OS.File.read
  |> Rresult.R.get_ok |> Official.Interf.pa
  |> Official.Interf.pr |> print_string

let pappx_interf_file f =
  f |> Fpath.v |> Bos.OS.File.read
  |> Rresult.R.get_ok |> PAPR.Interf.pa1 ~input_file:f
  |> NormLocations.signature |> List.map fst |> Ast2pt.interf f
  |> Official.Interf.pr |> print_string

let official_interf_string s =
  s |> Official.Interf.pa
  |> Official.Interf.pr |> print_string

let pappx_interf_string s =
  let f = "file://stashed" in
  Pa_ppx_dock.Pa_dock.stashed_file_contents := s ;
  s |> PAPR.Interf.pa1 ~input_file:f
  |> NormLocations.signature |> List.map fst |> Ast2pt.interf f
  |> Official.Interf.pr |> print_string

let%expect_test "minimal implem" =
#ifdef OFFICIAL
  official_implem_string
#else
  pappx_implem_string
#endif
 {|(** The first special comment of the file is the comment associated
     to the whole module. *)

 (** The comment for function f *)
 let f x y = x + y
|} ;
  [%expect {|
    [@@@ocaml.text
      " The first special comment of the file is the comment associated\n     to the whole module. "]
    let f x y = x + y[@@ocaml.doc " The comment for function f "] |}]

let%expect_test "minimal interf" =
#ifdef OFFICIAL
  official_interf_string
#else
  pappx_interf_string
#endif
{|(** The first special comment of the file is the comment associated
     with the whole module.*)


 (** Special comments can be placed between elements and are kept
     by the OCamldoc tool, but are not associated to any element.
     @-tags in these comments are ignored.*)

 (*******************************************************************)
 (** Comments like the one above, with more than two asterisks,
     are ignored. *)

 (** The comment for function f. *)
 val f : int -> int -> int
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
#ifdef OFFICIAL
  official_interf_string
#else
  pappx_interf_string
#endif
{|(** The first special comment of the file is the comment associated
     with the whole module.*)

 val f : int -> int -> int
|} ;
  [%expect {|
    [@@@ocaml.text
      " The first special comment of the file is the comment associated\n     with the whole module."]
    val f : int -> int -> int |}]

let%expect_test "implem: variants" =
#ifdef OFFICIAL
  official_implem_string
#else
  pappx_implem_string
#endif
 {|
  type t = A (** 1 *) | B (** 2 *) | C (** 3 *)
|} ;
  [%expect {|
    type t =
      | A [@ocaml.doc " 1 "]
      | B [@ocaml.doc " 2 "]
      | C [@ocaml.doc " 3 "]
 |}]
