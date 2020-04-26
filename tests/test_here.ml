open OUnit2

let here = [%here]

type position = [%import: Lexing.position] [@@deriving show]

let test_simplest ctxt =
let pos = {
    Lexing.pos_fname = "test_here.ml";
    pos_lnum = 3;
    pos_cnum = 24;
    pos_bol = 13
  } in
 assert_equal ~printer:show_position pos here

let suite = "Test here" >::: [
    "test_simplest"   >:: test_simplest
  ]

let _ = 
if Testutil2.invoked_with "test_here" || Testutil2.invoked_with "test_here.ppx" then
  run_test_tt_main suite
else ()
