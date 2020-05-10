let filemod = "Test_extensible_variants"
open OUnit2
open Sexplib.Sexp
open Sexplib.Std
open Sexplib0.Sexp_conv

type json =
  [ `Assoc of (string * json) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `Intlit of string
  | `List of json list
  | `Null
  | `String of string
  | `Tuple of json list
  | `Variant of string * json option ]
  [@@deriving show]

let show_error_or =
  let module M = struct
    type 'a error_or = ('a, string) Result.result [@@deriving show]
  end in
  M.show_error_or

let assert_roundtrip pp_obj to_json of_json obj str =
  let json = Yojson.Safe.from_string str in
  let cleanup json = Yojson.Safe.(json |> to_string |> from_string) in
  assert_equal ~printer:show_json json (cleanup (to_json obj));
  assert_equal ~printer:(show_error_or pp_obj) (Result.Ok obj) (of_json json)

let assert_roundtrip_sexp pp_obj to_sexp of_sexp obj str =
  let sexp = Sexplib.Sexp.of_string str in
  assert_equal ~printer:to_string_hum sexp (to_sexp obj);
  assert_equal ~printer:pp_obj obj (of_sexp sexp)

let assert_failure pp_obj of_json err str =
  let json = Yojson.Safe.from_string str in
  assert_equal ~printer:(show_error_or pp_obj) (Result.Error err) (of_json json)

type i1 = int         [@@deriving show, yojson, sexp, eq]

module A = struct
type t = ..
type t += A
end

module B = struct
  type u = A.t = .. [@@deriving show, sexp, yojson, eq]
end


type B.u += C [@rebind_to A.A ;] [@@deriving show, sexp, yojson, eq]

let test_AB ctxt =
  assert_equal ~printer:(fun x -> x) "Test_extensible_variants.C" (B.show_u C)
; assert_equal A.A C

module Exn = struct
type t = exn = .. [@@deriving show, sexp, yojson, eq]
end

type Exn.t +=
    Not_found [@rebind_to Stdlib.Not_found ;]
  | Failure of string [@rebind_to Stdlib.Failure ;]
[@@deriving show { with_path = false }, sexp, yojson, eq]

let test_exceptions ctxt =
  assert_equal Stdlib.Not_found Not_found
; assert_equal ~printer:(fun x -> x) "Not_found" (Exn.show Stdlib.Not_found)
; assert_equal (Stdlib.Failure "foo") (Failure "foo")
; assert_equal ~printer:(fun x -> x) {|(Failure "foo")|} (Exn.show (Stdlib.Failure "foo"))
; assert_roundtrip_sexp Exn.show Exn.sexp_of_t Exn.t_of_sexp
    Not_found "(Not_found)"
; assert_roundtrip_sexp Exn.show Exn.sexp_of_t Exn.t_of_sexp
    (Failure"foo") {|(Failure "foo")|}
; assert_roundtrip Exn.pp Exn.to_yojson Exn.of_yojson
    Not_found {|["Not_found"]|}
; assert_roundtrip Exn.pp Exn.to_yojson Exn.of_yojson
    (Failure"foo") {|["Failure", "foo"]|}
; assert_bool "equal-Not_found" (Exn.equal Stdlib.Not_found Not_found)
; assert_bool "not-equal-Not_found-Failure" (not (Exn.equal (Failure "foo") Not_found))

let suite = filemod >::: [
    "test_AB"  >:: test_AB;
    "test_exceptions"  >:: test_exceptions;
  ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()
