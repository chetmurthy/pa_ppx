open OUnit2

open Pa_ppx_dock

let test_simplest ctxt =
  let loc = Ploc.make_loc "" 1 0 (0, 0) "" in
  let s =
    {|(** The first special comment of the file is the comment associated
     with the whole module.*)

|} in
  let toks =  Comment_lexer.tokenize_comment (s, loc) in
  assert_equal ~printer:[%show: (string * int) list]
    [("(** The first special comment of the file is the comment associated\n     with the whole module.*)",
  0); ("\n\n", 97)] toks
    
let test_full_string ctxt =
  let s = {|(** The first special comment of the file is the comment associated
     with the whole module.*)

let x = 1
|} in
    let l = Pa_dock.comments_of_string s in
  assert_equal ~printer:[%show: (string * int) list]
    [("(** The first special comment of the file is the comment associated\n     with the whole module.*)",
  0); ("\n\n", 97); (" ", 102); (" ", 104); (" ", 106); ("\n", 108)] l

let suite = "Test here" >::: [
    "test_simplest"   >:: test_simplest
;   "test_full_string"   >:: test_full_string
  ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()
