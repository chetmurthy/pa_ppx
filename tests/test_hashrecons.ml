open OUnit2

type t = Leaf of int | Node of t * int * t

let rec deep = (function
  Leaf n[@hashrecons z] -> Leaf n[@hashrecons z]
| Node (l, n, r) [@hashrecons z] -> 
  Node (deep l, n, deep r) [@hashrecons z]
  )
[@@ocaml.warning "-26"]

let test_simplest ctxt =
 assert_equal (Node (Leaf 0, 1, Leaf 2)) (deep (Node (Leaf 0, 1, Leaf 2)))

let test_pointer_equality ctxt =
 let arg = Node (Leaf 0, 1, Leaf 2) in
 assert_bool "not pointer-equal" (arg == (deep arg))

let suite = "Test hashrecons" >::: [
    "test_simplest"   >:: test_simplest ;
    "test_pointer_equality"   >:: test_pointer_equality
  ]

let _ = 
if Testutil2.invoked_with "test_hashrecons" then
  run_test_tt_main suite
else ()
