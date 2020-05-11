open OUnit2

type t = [%import: Stuff.a]

type yy = [%import: Stuff.MI.i] [@@deriving show]
type zz = [%import: Stuff.i [@with MI.i := yy]] [@@deriving show]

module S = struct
  type w1 = [%import: Stuff.w1]
  and w2 = [%import: Stuff.w2 [@with i := zz]] [@@deriving show]
end

let _ = S.show_w2

[%%import: Stuff.w1 [@with i := zz]] [@@deriving show]

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
if not !Sys.interactive then
  run_test_tt_main suite
else ()
