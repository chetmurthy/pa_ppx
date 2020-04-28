open OUnit2

type t = [%import: Stuff.a]

module S = struct
  type w1 = [%import: Stuff.w1]
  and w2 = [%import: Stuff.w2 [@with i := Stuff.i]] [@@deriving show]
end

[%%import: Stuff.w1 [@with i := Stuff.i]] [@@deriving show]

module N = struct
type w1 =
   Stuff.w1 =
      A of w2 option[@with_deriving show]
and w2 =
  Stuff.w2 =
      B of w1 * Stuff.i[@with_deriving show]
end


let test_simplest ctxt =
 let (_ : t) = A1 in ()

let suite = "Test import(0)" >::: [
    "test_simplest"   >:: test_simplest
  ]

let _ = 
if Testutil2.invoked_with "test_import0" || Testutil2.invoked_with "test_import0.ppx" then
  run_test_tt_main suite
else ()
