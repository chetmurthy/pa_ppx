open OUnit2

open Pa_ppx_base
open HCPassthru

let pa entry s = s |> Stream.of_string |> Grammar.Entry.parse entry

let test_equality (entry,f) na s =
  na >:: (fun ctxt ->
      let arg = pa entry s in
      let ef = Pa_passthru.EF.mk() in
      let ctxt = Pa_passthru.Ctxt.mk ef Ploc.dummy in
      assert_bool ("equality: "^s) (arg = f ctxt arg)
    )

let test_pointer_equality (entry,f) na s =
  na >:: (fun ctxt ->
      let arg = pa entry s in
      let ef = Pa_passthru.EF.mk() in
      let ctxt = Pa_passthru.Ctxt.mk ef Ploc.dummy in
      assert_bool ("pointer equality: "^s) (arg == f ctxt arg)
    )

let suite = "Test passthru hashrecons" >::: [
    test_pointer_equality (Pcaml.expr, expr) "expr-0" "1"
    ; test_pointer_equality (Pcaml.expr, expr) "expr-0b" "1"
    ; test_pointer_equality (Pcaml.expr, expr) "expr-tup-0" "(1,2)"
    ; test_pointer_equality (Pcaml.str_item, str_item) "si-0" "(1,2)"
    ; test_pointer_equality (Pcaml.str_item, str_item) "si-typedecl-1" "type t = int"
  ]

let _ = 
if Testutil2.invoked_with "test_passthru_hashrecons" then
  run_test_tt_main suite
else ()
