
let is_even n = n mod 2 = 0

let%test _ = is_even 2

let%test _ = not (is_even 1)
let%test "busted" = not (is_even 10)
let%test_unit _ = ()
let%test_unit "busted-unit" = failwith "caught"

module F(A : sig val arg : bool end) = struct
  let %test "a" = A.arg
end

let%test_module _ = (module F(struct let arg = true end))
let%test_module "busted-mod" = (module F(struct let arg = false end))

let%expect_test _ =
  Printf.printf "%d" (1 + 2);
  [%expect {| 3 |}]

let%expect_test "busted-addition" =
  Printf.printf "%d" (1 + 2);
  [%expect {| 5 |}]

type position = [%import: Lexing.position] [@@deriving show]

let%expect_test "here" =
  pp_position Format.std_formatter [%here] ;
  [%expect {|
    { Lexing.pos_fname = "test_inline_test.ml"; pos_lnum = 29; pos_bol = 639;
      pos_cnum = 674 } |}]

let%test "name" [@tags "no-js"] = false
let%test "name" [@tags "no-js", "64-bits-only"] = false
let%test _ [@tags "no-js"] = false
let%test _ [@tags "js-only"] = false
let%test_module "name" [@tags "no-js"] = (module struct
  let %test "b" = false
end)
