open OUnit2

type t = Leaf of int | Node of t * int * t

module HCList = struct

let rec map f = function
    [][@hashrecons z] -> [][@hashrecons z]
  | (a::l)[@hashrecons z] -> let r = f a in ((r :: map f l)[@hashrecons z])
[@@ocaml.warning "-26"]
end

let deep =
let rec deep = (function
  Leaf n[@hashrecons z] -> Leaf n[@hashrecons z]
| Node (l, n, r) [@hashrecons z] -> 
  Node (deep l, n, deep r) [@hashrecons z]
  )
[@@ocaml.warning "-26"]
in deep

let test_tree_deep ctxt =
 assert_equal (Node (Leaf 0, 1, Leaf 2)) (deep (Node (Leaf 0, 1, Leaf 2)))

let test_tree_pointer_equality ctxt =
 let arg = Node (Leaf 0, 1, Leaf 2) in
 assert_bool "not pointer-equal" (arg == (deep arg))

type sexp = 
  | Atom of string 
  | List of sexp list

let sexp_deep =
  let rec deep = function
      Atom s[@hashrecons z] -> Atom s[@hashrecons z]
    | List l[@hashrecons z] -> List (HCList.map deep l)[@hashrecons z]
  in deep

let test_sexp_deep ctxt =
  let arg = List[Atom "lambda"; List[Atom"x"]; Atom "x"] in
 assert_equal arg (sexp_deep arg)

let test_sexp_pointer_equality0 ctxt =
  let arg = Atom "x" in
 assert_bool "not pointer-equal" (arg == (sexp_deep arg))

let test_sexp_pointer_equality1 ctxt =
  let arg = List[Atom "x"] in
 assert_bool "not pointer-equal" (arg == (sexp_deep arg))

let test_map0 ctxt =
  let arg = [] in
 assert_bool "not pointer-equal" (arg == HCList.map (fun x -> x) arg)

let test_map1 ctxt =
  let arg = [1;2;3] in
 assert_bool "not pointer-equal" (arg == HCList.map (fun x -> x) arg)

let test_sexp_pointer_equality2 ctxt =
  let arg = List[Atom "lambda"; List[Atom"x"]; Atom "x"] in
 assert_bool "not pointer-equal" (arg == (sexp_deep arg))

type tup3 = int * sexp * (string * bool) list

let tup3_deep = fun (a,b,c)[@hashrecons z] ->
  (a, sexp_deep b, HCList.map (fun (a,b)[@hashrecons y] -> (a,b)[@hashrecons y]) c)[@hashrecons z]

let test_tup3_pointer_equality1 ctxt =
  let arg = (1, List[Atom "lambda"; List[Atom"x"]; Atom "x"], [("foo", true)]) in
 assert_bool "not pointer-equal" (arg == (tup3_deep arg))

type rec2 = { a : int ; b : string }
let rec2_deep { a ; b }[@hashrecons z] = { a ; b}[@hashrecons z]


let test_rec2_pointer_equality1 ctxt =
  let arg = { a = 4 ; b = "foo" } in
 assert_bool "not pointer-equal" (arg == (rec2_deep arg))

let suite = "Test hashrecons" >::: [
    "test_tree_deep"   >:: test_tree_deep
  ; "test_tree_pointer_equality"   >:: test_tree_pointer_equality
  ; "test_sexp_deep"   >:: test_sexp_deep
  ; "test_map0"   >:: test_map0
  ; "test_map1"   >:: test_map1
  ; "test_sexp_pointer_equality0"   >:: test_sexp_pointer_equality0
  ; "test_sexp_pointer_equality1"   >:: test_sexp_pointer_equality1
  ; "test_sexp_pointer_equality2"   >:: test_sexp_pointer_equality2
  ; "test_tup3_pointer_equality1"   >:: test_tup3_pointer_equality1
  ; "test_rec2_pointer_equality1"   >:: test_rec2_pointer_equality1
  ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()
