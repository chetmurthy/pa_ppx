open OUnit2

let here = [%here]

type position = [%import: Lexing.position] [@@deriving show]

let test_simplest ctxt =
#ifdef PAPPX
let pos = {
    Lexing.pos_fname = "test_here.ml.pappx.ml";
    pos_lnum = 4;
    pos_cnum = 43;
    pos_bol = 32
  } in
#else
let pos = {
    Lexing.pos_fname = "test_here.ml";
    pos_lnum = 3;
    pos_cnum = 43;
    pos_bol = 32
  } in
#endif
 assert_equal ~printer:show_position pos here

let suite = "Test here" >::: [
    "test_simplest"   >:: test_simplest
  ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()
