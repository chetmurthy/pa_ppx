open OUnit2

type t = [%import: Stuff.t]

let test_simplest ctxt =
 let (_ : bool) = true in ()

let suite = "Test import(0)" >::: [
    "test_simplest"   >:: test_simplest
  ]

let _ = 
if Testutil2.invoked_with "test_import0" || Testutil2.invoked_with "test_import0.ppx" then
  run_test_tt_main suite
else ()
