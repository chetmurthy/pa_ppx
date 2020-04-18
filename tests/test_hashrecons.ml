open OUnit2

type t = Leaf of int | Node of t * int * t

let deep =
let rec deep = (function
  Leaf n[@hashrecons z] -> Leaf n[@hashrecons z]
| Node (l, n, r) [@hashrecons z] -> 
  Node (deep l, n, deep r) [@hashrecons z]
  )
[@@ocaml.warning "-26"]
in deep

let rec manual_deep =
let rec deep =
  (function
    | Leaf (n as z_0) as z_1 ->
      let cz_1 =
        let cz_0 = n in
        if cz_0 == z_0 && true then z_1 else Leaf cz_0
      in cz_1

    | Node ((l as z_0), (n as z_1), (r as z_2)) as z_3 ->
      let cz_3 =
        let cz_0 = deep l in
        let cz_1 = n in
        let cz_2 = deep r in
        if cz_0 == z_0 && cz_1 == z_1 && cz_2 == z_2 && true then z_3
        else Node (cz_0, cz_1, cz_2)
      in cz_3

  )[@@ocaml.warning "-26"]
in deep

let test_simplest ctxt =
 assert_equal (Node (Leaf 0, 1, Leaf 2)) (deep (Node (Leaf 0, 1, Leaf 2)))

let test_pointer_equality ctxt =
 let arg = Node (Leaf 0, 1, Leaf 2) in
 assert_bool "not pointer-equal" (arg == (deep arg))

let test_manual_pointer_equality0 ctxt =
 let arg = Leaf 0 in
 assert_bool "not pointer-equal" (arg == (manual_deep arg))

let test_manual_pointer_equality1 ctxt =
 let arg = Node (Leaf 0, 1, Leaf 2) in
 assert_bool "not pointer-equal" (arg == (manual_deep arg))

let suite = "Test hashrecons" >::: [
    "test_simplest"   >:: test_simplest ;
    "test_pointer_equality"   >:: test_pointer_equality ;
    "test_manual_pointer_equality0"   >:: test_manual_pointer_equality0 ;
    "test_manual_pointer_equality1"   >:: test_manual_pointer_equality1
  ]

let _ = 
if Testutil2.invoked_with "test_hashrecons" then
  run_test_tt_main suite
else ()
