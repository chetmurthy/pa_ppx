open OUnit2


module Ploc = struct
include Ploc

let pp ppf x = Fmt.(const string "<loc>" ppf ())

type 'a vala = [%import: 'a Ploc.vala] [@@deriving show]
end

type loc = [%import: MLast.loc] [@@deriving show]
type type_var = [%import: MLast.type_var] [@@deriving show]

[%%import: MLast.expr] [@@deriving show]

let test_simplest ctxt =
  ()

let suite = "Test import_camlp5" >::: [
    "test_simplest"   >:: test_simplest
  ]
let _ = 
if Testutil2.invoked_with "test_import_caml5" then
  run_test_tt_main suite
else ()
