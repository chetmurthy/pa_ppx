let filemod = "Test_extensible_variants"
open OUnit2
open Pa_ppx_runtime

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
  assert_equal ~printer:Sexplib0.Sexp.to_string_hum sexp (to_sexp obj);
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

module Exn = Exceptions

let test_exceptions ctxt =
  assert_equal Stdlib.Not_found Not_found
; assert_equal ~printer:(fun x -> x) "Stdlib.Not_found" (Exn.show Stdlib.Not_found)
; assert_equal (Stdlib.Failure "foo") (Failure "foo")
; assert_equal ~printer:(fun x -> x) {|(Stdlib.Failure "foo")|} (Exn.show (Stdlib.Failure "foo"))
; assert_roundtrip_sexp Exn.show Exn.sexp_of_t Exn.t_of_sexp
    Not_found "(Stdlib.Not_found)"
; assert_roundtrip_sexp Exn.show Exn.sexp_of_t Exn.t_of_sexp
    (Failure"foo") {|(Stdlib.Failure "foo")|}
; assert_roundtrip Exn.pp Exn.to_yojson Exn.of_yojson
    Not_found {|["Stdlib.Not_found"]|}
; assert_roundtrip Exn.pp Exn.to_yojson Exn.of_yojson
    (Failure"foo") {|["Stdlib.Failure", "foo"]|}
; assert_bool "equal-Not_found" (Exn.equal Stdlib.Not_found Not_found)
; assert_bool "not-equal-Not_found-Failure" (not (Exn.equal (Failure "foo") Not_found))

exception Y of string

type Exn.t +=
    XY of string [@rebind_to Y ;][@name "Y";]
[@@deriving show { with_path = false }, sexp, yojson, eq]

let test_exceptions2 ctxt =
  assert_equal ~printer:Sexplib.Sexp.to_string_hum
    Sexplib.Sexp.(List [Atom"Y"; Atom"foo"])
    (Exn.sexp_of_t (XY"foo"))
; assert_roundtrip_sexp Exn.show Exn.sexp_of_t Exn.t_of_sexp
    (XY"foo") {|(Y "foo")|}

module MZ = struct
  exception Z of string
end

exception Z of string [@name "MZ.Z"] [@rebind_to MZ.Z]  [@@deriving show, eq, yojson, sexp]

let test_exceptions3 ctxt =
  assert_equal ~printer:(fun x -> x)
    {|(MZ.Z "foo")|} (Exn.show (Z"foo"))
; assert_bool "equal-MZ.Z" (Exn.equal (MZ.Z "foo") (MZ.Z "foo"))
; assert_roundtrip Exn.pp Exn.to_yojson Exn.of_yojson
    (Z"foo") {|["MZ.Z", "foo"]|}
; assert_roundtrip_sexp Exn.show Exn.sexp_of_t Exn.t_of_sexp
    (Z"foo") {|(MZ.Z "foo")|}

let suite = filemod >::: [
    "test_AB"  >:: test_AB;
    "test_exceptions"  >:: test_exceptions;
    "test_exceptions2"  >:: test_exceptions2;
    "test_exceptions3"  >:: test_exceptions3;
  ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()
