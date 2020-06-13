#ifdef PAPPX
let filemod = "Test_syntax.ml"
#else
let filemod = "Test_syntax.ml.ppx"
#endif
open OUnit2
open Pa_ppx_utils

type uint32 = Uint32.t
type uint64 = Uint64.t

let assert_roundtrip printer encoder decoder str value =
  let fmt_hex_string s = Hash.hex_of_bytes (Bytes.of_string s) in
  (* encode *)
  let e = Protobuf.Encoder.create () in
  encoder value e;
  assert_equal ~printer:fmt_hex_string str (Protobuf.Encoder.to_string e);
  (* decode *)
  let d = Protobuf.Decoder.of_string str in
  assert_equal ~printer value (decoder d)

let assert_differential_roundtrip printer encoder decoder str in_value out_value =
  let fmt_hex_string s = Hash.hex_of_bytes (Bytes.of_string s) in
  (* encode *)
  let e = Protobuf.Encoder.create () in
  encoder in_value e;
  assert_equal ~printer:fmt_hex_string str (Protobuf.Encoder.to_string e);
  (* decode *)
  let d = Protobuf.Decoder.of_string str in
  assert_equal ~printer out_value (decoder d)

type b = bool [@@deriving protobuf]
let test_bool ctxt =
  assert_roundtrip string_of_bool b_to_protobuf b_from_protobuf
                   "\x08\x01" true

type i1  = int                       [@@deriving protobuf, show]
type i1'  = int [@key 2]             [@@deriving protobuf]
type i2  = int   [@encoding `zigzag] [@@deriving protobuf]
type i3  = int   [@encoding `bits32] [@@deriving protobuf]
type i4  = int   [@encoding `bits64] [@@deriving protobuf]
type il1 = int32 [@encoding `varint] [@@deriving protobuf]
type il2 = int32 [@encoding `zigzag] [@@deriving protobuf]
type il3 = Int32.t                   [@@deriving protobuf]
type il4 = int32 [@encoding `bits64] [@@deriving protobuf]
type iL1 = int64 [@encoding `varint] [@@deriving protobuf]
type iL2 = int64 [@encoding `zigzag] [@@deriving protobuf]
type iL3 = int64 [@encoding `bits32] [@@deriving protobuf]
type iL4 = Int64.t                   [@@deriving protobuf]
let test_ints ctxt =
  assert_roundtrip string_of_int i1_to_protobuf i1_from_protobuf
                   "\x08\xac\x02" 300;
  assert_roundtrip string_of_int i1'_to_protobuf i1'_from_protobuf
                   "\x10\xac\x02" 300;
  assert_roundtrip string_of_int i2_to_protobuf i2_from_protobuf
                   "\x08\xac\x02" 150;
  assert_roundtrip string_of_int i3_to_protobuf i3_from_protobuf
                   "\x0d\x2c\x01\x00\x00" 300;
  assert_roundtrip string_of_int i4_to_protobuf i4_from_protobuf
                   "\x09\x2c\x01\x00\x00\x00\x00\x00\x00" 300;

  assert_roundtrip Int32.to_string il1_to_protobuf il1_from_protobuf
                   "\x08\xac\x02" 300l;
  assert_roundtrip Int32.to_string il2_to_protobuf il2_from_protobuf
                   "\x08\xac\x02" 150l;
  assert_roundtrip Int32.to_string il3_to_protobuf il3_from_protobuf
                   "\x0d\x2c\x01\x00\x00" 300l;
  assert_roundtrip Int32.to_string il4_to_protobuf il4_from_protobuf
                   "\x09\x2c\x01\x00\x00\x00\x00\x00\x00" 300l;

  assert_roundtrip Int64.to_string iL1_to_protobuf iL1_from_protobuf
                   "\x08\xac\x02" 300L;
  assert_roundtrip Int64.to_string iL2_to_protobuf iL2_from_protobuf
                   "\x08\xac\x02" 150L;
  assert_roundtrip Int64.to_string iL3_to_protobuf iL3_from_protobuf
                   "\x0d\x2c\x01\x00\x00" 300L;
  assert_roundtrip Int64.to_string iL4_to_protobuf iL4_from_protobuf
                   "\x09\x2c\x01\x00\x00\x00\x00\x00\x00" 300L

type ul1 = uint32 [@encoding `varint] [@@deriving protobuf]
type ul2 = uint32 [@encoding `zigzag] [@@deriving protobuf]
type ul3 = Uint32.t                   [@@deriving protobuf]
type ul4 = uint32 [@encoding `bits64] [@@deriving protobuf]
type uL1 = uint64 [@encoding `varint] [@@deriving protobuf]
type uL2 = uint64 [@encoding `zigzag] [@@deriving protobuf]
type uL3 = uint64 [@encoding `bits32] [@@deriving protobuf]
type uL4 = Uint64.t                   [@@deriving protobuf]
let test_uints ctxt =
  assert_roundtrip Uint32.to_string ul1_to_protobuf ul1_from_protobuf
                   "\x08\xac\x02" (Uint32.of_int 300);
  assert_roundtrip Uint32.to_string ul2_to_protobuf ul2_from_protobuf
                   "\x08\xac\x02" (Uint32.of_int 150);
  assert_roundtrip Uint32.to_string ul3_to_protobuf ul3_from_protobuf
                   "\x0d\x2c\x01\x00\x00" (Uint32.of_int 300);
  assert_roundtrip Uint32.to_string ul4_to_protobuf ul4_from_protobuf
                   "\x09\x2c\x01\x00\x00\x00\x00\x00\x00" (Uint32.of_int 300);

  assert_roundtrip Uint64.to_string uL1_to_protobuf uL1_from_protobuf
                   "\x08\xac\x02" (Uint64.of_int 300);
  assert_roundtrip Uint64.to_string uL2_to_protobuf uL2_from_protobuf
                   "\x08\xac\x02" (Uint64.of_int 150);
  assert_roundtrip Uint64.to_string uL3_to_protobuf uL3_from_protobuf
                   "\x0d\x2c\x01\x00\x00" (Uint64.of_int 300);
  assert_roundtrip Uint64.to_string uL4_to_protobuf uL4_from_protobuf
                   "\x09\x2c\x01\x00\x00\x00\x00\x00\x00" (Uint64.of_int 300)

type f1 = float [@encoding `bits32] [@@deriving protobuf]
type f2 = float                    [@@deriving protobuf]
type f3 = float [@encoding `bits64] [@@deriving protobuf]
let test_floats ctxt =
  assert_roundtrip string_of_float f1_to_protobuf f1_from_protobuf
                   "\x0d\x00\x00\xC0\x3f" 1.5;
  assert_roundtrip string_of_float f2_to_protobuf f2_from_protobuf
                   "\x09\x00\x00\x00\x00\x00\x00\xF8\x3f" 1.5;
  assert_roundtrip string_of_float f3_to_protobuf f3_from_protobuf
                   "\x09\x00\x00\x00\x00\x00\x00\xF8\x3f" 1.5

type s = string [@@deriving protobuf]
let test_string ctxt =
  assert_roundtrip (fun x -> x) s_to_protobuf s_from_protobuf
                   "\x0a\x03abc" "abc"

type by = bytes [@@deriving protobuf]
let test_bytes ctxt =
  assert_roundtrip (fun x -> Bytes.to_string x) by_to_protobuf by_from_protobuf
                   "\x0a\x03abc" (Bytes.of_string "abc")

type o = int option [@@deriving protobuf]
let test_option ctxt =
  let printer x = match x with None -> "None" | Some v -> "Some " ^ (string_of_int v) in
  assert_roundtrip printer o_to_protobuf o_from_protobuf
                   "" None;
  assert_roundtrip printer o_to_protobuf o_from_protobuf
                   "\x08\xac\x02" (Some 300)

type l = int list [@@deriving protobuf]
let test_list ctxt =
  let printer x = x |> List.map string_of_int |> String.concat ", " in
  assert_roundtrip printer l_to_protobuf l_from_protobuf
                   "" [] ;
  assert_roundtrip printer l_to_protobuf l_from_protobuf
                   "\x08\xac\x02\x08\x2a" [300; 42] ;
  assert_differential_roundtrip string_of_int l_to_protobuf i1_from_protobuf
                   "\x08\xac\x02\x08\x2a" [300; 42] 42

type a = int array [@@deriving protobuf]
let test_array ctxt =
  let printer x = Array.to_list x |> List.map string_of_int |> String.concat ", " in
  assert_roundtrip printer a_to_protobuf a_from_protobuf
                   "" [||];
  assert_roundtrip printer a_to_protobuf a_from_protobuf
                   "\x08\xac\x02\x08\x2a" [|300; 42|]

type ts = int * string [@@deriving protobuf]
let ts_printer (x, y) = Printf.sprintf "%d, %s" x y 
let test_tuple ctxt =
  assert_roundtrip ts_printer ts_to_protobuf ts_from_protobuf
                   "\x08\xac\x02\x12\x08spartans" (300, "spartans")

type ts' = (string[@key 2]) * (int[@key 1]) [@@deriving protobuf]
let ts'_printer (y, x) = Printf.sprintf "%s, %d" y x 
let test_tuple' ctxt =
  assert_roundtrip ts'_printer ts'_to_protobuf ts'_from_protobuf
                   "\x08\xac\x02\x12\x08spartans" ("spartans", 300)
; assert_differential_roundtrip ts'_printer ts_to_protobuf ts'_from_protobuf
                   "\x08\xac\x02\x12\x08spartans" (300, "spartans") ("spartans", 300)

type ts'' = ts option [@@deriving protobuf]
let ts''_printer = function None -> "<>" | Some (x, y) -> Printf.sprintf "(%d, %s)" x y 
let test_ts'' ctxt =
  assert_roundtrip ts''_printer ts''_to_protobuf ts''_from_protobuf
                   "\n\r\b\172\002\018\bspartans" (Some(300, "spartans"))
; assert_roundtrip ts''_printer ts''_to_protobuf ts''_from_protobuf
                   "" None

type tup3 = (string[@key 3]) * (int[@key 2]) * (int option [@key 1]) [@@deriving protobuf]
let tup3_printer (x, y, z) = Printf.sprintf "%s, %d, %s" x y (match z with None -> "<>" | Some n -> string_of_int n)

let test_tup3 ctxt =
  assert_roundtrip tup3_printer tup3_to_protobuf tup3_from_protobuf
                   "\b*\016\172\002\026\bspartans" ("spartans", 300, Some 42)

type tsts = (int * string) * string [@@deriving protobuf]
let tsts_printer (x, y) = Printf.sprintf "%s, %s" (ts_printer x) y 
let test_tsts ctxt =
  assert_roundtrip tsts_printer tsts_to_protobuf tsts_from_protobuf
                   "\n\b\b\172\002\018\003foo\018\bspartans" ((300, "foo"), "spartans")

type tsts' = ts * string [@@deriving protobuf]
let test_tsts' ctxt =
  assert_roundtrip tsts_printer tsts'_to_protobuf tsts'_from_protobuf
                   "\n\b\b\172\002\018\003foo\018\bspartans" ((300, "foo"), "spartans")

type tsts'' = ts option * string [@@deriving protobuf]
let tsts''_printer (x, y) = Printf.sprintf "%s, %s" (ts''_printer x) y 
let test_tsts'' ctxt =
  assert_roundtrip tsts''_printer tsts''_to_protobuf tsts''_from_protobuf
                   "\n\b\b\172\002\018\003foo\018\bspartans" (Some (300, "foo"), "spartans")

type r1 = {
  r1a : int    [@key 1];
  r1b : string [@key 2];
} [@@deriving protobuf]
let test_record ctxt =
  let printer r = Printf.sprintf "{ r1a = %d, r1b = %s }" r.r1a r.r1b in
  assert_roundtrip printer r1_to_protobuf r1_from_protobuf
                   "\x08\xac\x02\x12\x08spartans"
                   { r1a = 300; r1b = "spartans" }
type r2 = {
  r2a : r1 [@key 1];
} [@@deriving protobuf]
let test_nested ctxt =
  let printer r = Printf.sprintf "{ r2a = { r1a = %d, r1b = %s } }" r.r2a.r1a r.r2a.r1b in
  assert_roundtrip printer r2_to_protobuf r2_from_protobuf
                   "\x0a\x0d\x08\xac\x02\x12\x08spartans"
                   { r2a = { r1a = 300; r1b = "spartans" } }

type r3 = {
  r3a : (int [@encoding `bits32]) * string [@key 1];
} [@@deriving protobuf]
let test_imm_tuple ctxt =
  let printer { r3a = a, b } = Printf.sprintf "{ r3a = %d, %s } }" a b in
  assert_roundtrip printer r3_to_protobuf r3_from_protobuf
                   "\x0a\x0f\x0d\x2c\x01\x00\x00\x12\x08spartans"
                   { r3a = 300, "spartans" }

type r3' = {
  r3a : (int [@encoding `bits32]) * string [@key 1];
  r3b : bool * bytes [@key 2];
} [@@deriving protobuf]

type v1 =
| V1A [@key 1]
| V1B [@key 2]
| V1C of int [@key 3]
| V1D of string * string [@key 4]
[@@deriving protobuf]
let test_variant ctxt =
  let printer v =
    match v with
    | V1A -> "V1A"
    | V1B -> "V1B"
    | V1C i -> Printf.sprintf "V1C(%d)" i
    | V1D (s1,s2) -> Printf.sprintf "V1D(%S, %S)" s1 s2
  in
  assert_roundtrip printer v1_to_protobuf v1_from_protobuf
                   "\x08\x02" V1B;
  assert_roundtrip printer v1_to_protobuf v1_from_protobuf
                   "\x08\x03\x20\x2a" (V1C 42);
  assert_roundtrip printer v1_to_protobuf v1_from_protobuf
                   "\x08\x04\x2a\x0a\x0a\x03foo\x12\x03bar" (V1D ("foo", "bar"))

type v6 =
| V6A [@key 1]
| V6B [@key 2]
[@@deriving protobuf]
let test_v6 ctxt =
  let printer v =
    match v with
    | V6A -> "V6A"
    | V6B -> "V6B"
  in
  assert_roundtrip printer v6_to_protobuf v6_from_protobuf
                   "\x08\x02" V6A;
  ()
#ifndef PAPPX
type v2 =
| V2A [@key 1]
| V2B [@key 2]
and r4 = {
  r4a : v2 [@key 1] [@bare]
} [@@deriving protobuf]
let test_variant_bare ctxt =
  let printer { r4a } =
    match r4a with V2A -> "{ r4a = V2A }" | V2B -> "{ r4a = V2B }"
  in
  assert_roundtrip printer r4_to_protobuf r4_from_protobuf
                   "\x08\x02" { r4a = V2B }
#endif

type 'a r5 = {
  r5a: 'a [@key 1]
} [@@deriving protobuf]
let test_tvar ctxt =
  let printer f { r5a } = Printf.sprintf "{ r5a = %s }" (f r5a) in
  assert_roundtrip (printer string_of_int)
                   (r5_to_protobuf i1_to_protobuf)
                   (r5_from_protobuf i1_from_protobuf)
                   "\x0a\x02\x08\x01" { r5a = 1 }

type 'a mylist =
| Nil [@key 1]
| Cons of 'a * 'a mylist [@key 2]
[@@deriving protobuf]
let test_mylist ctxt =
  let rec printer f v =
    match v with
    | Nil -> "Nil"
    | Cons (a, r) -> Printf.sprintf "Cons (%s, %s)" (f a) (printer f r)
  in
  assert_roundtrip (printer string_of_int)
                   (mylist_to_protobuf i1_to_protobuf)
                   (mylist_from_protobuf i1_from_protobuf)
                   "\b\001"
                   Nil ;
  assert_roundtrip (printer string_of_int)
                   (mylist_to_protobuf i1_to_protobuf)
                   (mylist_from_protobuf i1_from_protobuf)
                   "\b\002\026\b\n\002\b\003\018\002\b\001"
                   (Cons (3, Nil)) ;
  assert_roundtrip (printer string_of_int)
                   (mylist_to_protobuf i1_to_protobuf)
                   (mylist_from_protobuf i1_from_protobuf)
                   "\b\002\026\018\n\002\b\002\018\012\b\002\026\b\n\002\b\003\018\002\b\001"
                   (Cons (2, (Cons (3, Nil)))) ;
  assert_roundtrip (printer string_of_int)
                   (mylist_to_protobuf i1_to_protobuf)
                   (mylist_from_protobuf i1_from_protobuf)
                   ("\x08\x02\x1a\x1c\x0a\x02\x08\x01\x12\x16\x08\x02" ^
                    "\x1a\x12\x0a\x02\x08\x02\x12\x0c\x08\x02\x1a\x08" ^
                    "\x0a\x02\x08\x03\x12\x02\x08\x01")
                   (Cons (1, (Cons (2, (Cons (3, Nil))))))

type v3 = [
  `V3A [@key 1]
| `V3B of int [@key 2]
| `V3C of string * string [@key 3]
]
[@@deriving protobuf]
let test_poly_variant ctxt =
  let printer v =
    match v with
    | `V3A -> "`V3A"
    | `V3B i -> Printf.sprintf "`V3B(%d)" i
    | `V3C (s1,s2) -> Printf.sprintf "`V3C(%S, %S)" s1 s2
  in
  assert_roundtrip printer v3_to_protobuf v3_from_protobuf
                   "\x08\x01" `V3A;
  assert_roundtrip printer v3_to_protobuf v3_from_protobuf
                   "\x08\x02\x18\x2a" (`V3B 42);
  assert_roundtrip printer v3_to_protobuf v3_from_protobuf
                   "\x08\x03\x22\x0a\x0a\x03abc\x12\x03def" (`V3C ("abc", "def"))

type r6 = {
  r6a : [ `R6A [@key 1] | `R6B [@key 2] ] [@key 1];
} [@@deriving protobuf]
let r6_printer { r6a } =
    match r6a with `R6A -> "{ r6a = `R6A }" | `R6B -> "{ r6a = `R6B }"

let test_imm_pvariant ctxt =
  assert_roundtrip r6_printer r6_to_protobuf r6_from_protobuf
                   "\x0a\x02\x08\x02" { r6a = `R6B }

#ifndef PAPPX
type v4 = [ `V4A [@key 1] | `V4B [@key 2] ]
and r7 = {
  r7a : v4 [@key 1] [@bare]
} [@@deriving protobuf]
let test_pvariant_bare ctxt =
  let printer { r7a } =
    match r7a with `V4A -> "{ r7a = `V4A }" | `V4B -> "{ r7a = `V4B }"
  in
  assert_roundtrip printer r7_to_protobuf r7_from_protobuf
                   "\x08\x01" { r7a = `V4A }

type r8 = {
  r8a : [ `Request [@key 1] | `Reply [@key 2] ] [@key 1] [@bare];
  r8b : int [@key 2];
} [@@deriving protobuf]
let test_imm_pv_bare ctxt =
  let printer { r8a; r8b } =
    match r8a with
    | `Request -> Printf.sprintf "{ r8a = `Request; r8b = %d }" r8b
    | `Reply   -> Printf.sprintf "{ r8a = `Reply; r8b = %d }" r8b
  in
  assert_roundtrip printer r8_to_protobuf r8_from_protobuf
                   "\x08\x01\x10\x2a" { r8a = `Request; r8b = 42 }
#endif

type 'a optionmsg = { it_option : 'a option [@key 1] }[@@deriving show, protobuf]
let test_optionmsg ctxt =
  assert_roundtrip (show_optionmsg pp_i1) (optionmsg_to_protobuf i1_to_protobuf) (optionmsg_from_protobuf i1_from_protobuf)
                   "" { it_option = None }
; assert_roundtrip (show_optionmsg pp_i1) (optionmsg_to_protobuf i1_to_protobuf) (optionmsg_from_protobuf i1_from_protobuf)
                   "\x0a\x03\x08\xac\x02" { it_option = Some 300 }

type 'a listmsg = { it_list : 'a list [@key 1] }[@@deriving show, protobuf]
let test_listmsg ctxt =
  assert_roundtrip (show_listmsg pp_i1) (listmsg_to_protobuf i1_to_protobuf) (listmsg_from_protobuf i1_from_protobuf)
                   "" { it_list = [] }
; assert_roundtrip (show_listmsg pp_i1) (listmsg_to_protobuf i1_to_protobuf) (listmsg_from_protobuf i1_from_protobuf)
                   "\x0a\x03\x08\xac\x02" { it_list = [300] }

type 'a arraymsg = { it_array : 'a array [@key 1] }[@@deriving show, protobuf]
let test_arraymsg ctxt =
  assert_roundtrip (show_arraymsg pp_i1) (arraymsg_to_protobuf i1_to_protobuf) (arraymsg_from_protobuf i1_from_protobuf)
                   "" { it_array = [||] }
; assert_roundtrip (show_arraymsg pp_i1) (arraymsg_to_protobuf i1_to_protobuf) (arraymsg_from_protobuf i1_from_protobuf)
                   "\x0a\x03\x08\xac\x02" { it_array = [|300|] }

type i1om = i1 optionmsg [@@deriving show, protobuf]
let test_i1om ctxt =
  assert_roundtrip show_i1om i1om_to_protobuf i1om_from_protobuf
                   "\x0a\x00" {it_option = None}
; assert_roundtrip show_i1om i1om_to_protobuf i1om_from_protobuf
                   "\x0a\x05\x0a\x03\x08\xac\x02" { it_option = (Some 300) }

type i1omo = i1 optionmsg option [@@deriving show, protobuf]
let test_i1omo ctxt =
  assert_roundtrip show_i1omo i1omo_to_protobuf i1omo_from_protobuf
                   "" None
; assert_roundtrip show_i1omo i1omo_to_protobuf i1omo_from_protobuf
                   "\x0a\x00" (Some {it_option = None})
; assert_roundtrip show_i1omo i1omo_to_protobuf i1omo_from_protobuf
                   "\x0a\x05\x0a\x03\x08\xac\x02" (Some { it_option = (Some 300) })

type i1oml = i1 optionmsg list [@@deriving show, protobuf]
let test_i1oml ctxt =
  assert_roundtrip show_i1oml i1oml_to_protobuf i1oml_from_protobuf
                   "" []
; assert_roundtrip show_i1oml i1oml_to_protobuf i1oml_from_protobuf
                   "\x0a\x00" [{it_option = None}]
; assert_roundtrip show_i1oml i1oml_to_protobuf i1oml_from_protobuf
                   "\x0a\x05\x0a\x03\x08\xac\x02" [{ it_option = (Some 300) }]
; assert_roundtrip show_i1oml i1oml_to_protobuf i1oml_from_protobuf
                   "\x0a\x05\x0a\x03\x08\xac\x02\x0a\x04\x0a\x02\x08\x2a" [{ it_option = (Some 300) }; { it_option = (Some 42) }]

type i1oma = i1 optionmsg array [@@deriving show, protobuf]
let test_i1oma ctxt =
  assert_roundtrip show_i1oma i1oma_to_protobuf i1oma_from_protobuf
                   "" [||]
; assert_roundtrip show_i1oma i1oma_to_protobuf i1oma_from_protobuf
                   "\x0a\x00" [|{it_option = None}|]
; assert_roundtrip show_i1oma i1oma_to_protobuf i1oma_from_protobuf
                   "\x0a\x05\x0a\x03\x08\xac\x02" [|{ it_option = (Some 300) }|]
; assert_roundtrip show_i1oma i1oma_to_protobuf i1oma_from_protobuf
                   "\x0a\x05\x0a\x03\x08\xac\x02\x0a\x04\x0a\x02\x08\x2a" [|{ it_option = (Some 300) }; { it_option = (Some 42) }|]

type i1lm = i1 listmsg [@@deriving show, protobuf]
let test_i1lm ctxt =
  assert_roundtrip show_i1lm i1lm_to_protobuf i1lm_from_protobuf
                   "\x0a\x00" {it_list = []}
; assert_roundtrip show_i1lm i1lm_to_protobuf i1lm_from_protobuf
                   "\x0a\x05\x0a\x03\x08\xac\x02" { it_list = [300] }
; assert_roundtrip show_i1lm i1lm_to_protobuf i1lm_from_protobuf
                   "\x0a\x09\x0a\x03\x08\xac\x02\x0a\x02\x08\x2a" { it_list = [300; 42] }

type i1lmo = i1 listmsg option [@@deriving show, protobuf]
let test_i1lmo ctxt =
  assert_roundtrip show_i1lmo i1lmo_to_protobuf i1lmo_from_protobuf
                   "" None
; assert_roundtrip show_i1lmo i1lmo_to_protobuf i1lmo_from_protobuf
                   "\x0a\x05\x0a\x03\x08\xac\x02" (Some { it_list = [300] })
; assert_roundtrip show_i1lmo i1lmo_to_protobuf i1lmo_from_protobuf
                   "\x0a\x09\x0a\x03\x08\xac\x02\x0a\x02\x08\x2a" (Some { it_list = [300; 42] })

type i1lml = i1 listmsg list [@@deriving show, protobuf]
let test_i1lml ctxt =
  assert_roundtrip show_i1lml i1lml_to_protobuf i1lml_from_protobuf
                   "" []
; assert_roundtrip show_i1lml i1lml_to_protobuf i1lml_from_protobuf
                   "\x0a\x05\x0a\x03\x08\xac\x02" [{ it_list = [300] }]
; assert_roundtrip show_i1lml i1lml_to_protobuf i1lml_from_protobuf
                   "\x0a\x09\x0a\x03\x08\xac\x02\x0a\x02\x08\x2a" [{ it_list = [300; 42] }]
; assert_roundtrip show_i1lml i1lml_to_protobuf i1lml_from_protobuf
                   "\x0a\x09\x0a\x03\x08\xac\x02\x0a\x02\x08\x2a\x0a\x09\x0a\x03\x08\x90\x03\x0a\x02\x08\x16" [{ it_list = [300; 42] }; { it_list = [400; 22] }]

type i1lma = i1 listmsg array [@@deriving show, protobuf]
let test_i1lma ctxt =
  assert_roundtrip show_i1lma i1lma_to_protobuf i1lma_from_protobuf
                   "" [||]
; assert_roundtrip show_i1lma i1lma_to_protobuf i1lma_from_protobuf
                   "\x0a\x05\x0a\x03\x08\xac\x02" [|{ it_list = [300] }|]
; assert_roundtrip show_i1lma i1lma_to_protobuf i1lma_from_protobuf
                   "\x0a\x09\x0a\x03\x08\xac\x02\x0a\x02\x08\x2a" [|{ it_list = [300; 42] }|]
; assert_roundtrip show_i1lma i1lma_to_protobuf i1lma_from_protobuf
                   "\x0a\x09\x0a\x03\x08\xac\x02\x0a\x02\x08\x2a\x0a\x09\x0a\x03\x08\x90\x03\x0a\x02\x08\x16" [|{ it_list = [300; 42] }; { it_list = [400; 22] }|]

type i1am = i1 arraymsg [@@deriving show, protobuf]
let test_i1am ctxt =
  assert_roundtrip show_i1am i1am_to_protobuf i1am_from_protobuf
                   "\x0a\x00" {it_array = [||]}
; assert_roundtrip show_i1am i1am_to_protobuf i1am_from_protobuf
                   "\x0a\x05\x0a\x03\x08\xac\x02" { it_array = [|300|] }
; assert_roundtrip show_i1am i1am_to_protobuf i1am_from_protobuf
                   "\x0a\x09\x0a\x03\x08\xac\x02\x0a\x02\x08\x2a" { it_array = [|300; 42|] }

type i1amo = i1 arraymsg option [@@deriving show, protobuf]
let test_i1amo ctxt =
  assert_roundtrip show_i1amo i1amo_to_protobuf i1amo_from_protobuf
                   "" None
; assert_roundtrip show_i1amo i1amo_to_protobuf i1amo_from_protobuf
                   "\x0a\x05\x0a\x03\x08\xac\x02" (Some { it_array = [|300|] })
; assert_roundtrip show_i1amo i1amo_to_protobuf i1amo_from_protobuf
                   "\x0a\x09\x0a\x03\x08\xac\x02\x0a\x02\x08\x2a" (Some { it_array = [|300; 42|] })

type i1aml = i1 arraymsg list [@@deriving show, protobuf]
let test_i1aml ctxt =
  assert_roundtrip show_i1aml i1aml_to_protobuf i1aml_from_protobuf
                   "" []
; assert_roundtrip show_i1aml i1aml_to_protobuf i1aml_from_protobuf
                   "\x0a\x05\x0a\x03\x08\xac\x02" [{ it_array = [|300|] }]
; assert_roundtrip show_i1aml i1aml_to_protobuf i1aml_from_protobuf
                   "\x0a\x09\x0a\x03\x08\xac\x02\x0a\x02\x08\x2a" [{ it_array = [|300; 42|] }]
; assert_roundtrip show_i1aml i1aml_to_protobuf i1aml_from_protobuf
                   "\x0a\x09\x0a\x03\x08\xac\x02\x0a\x02\x08\x2a\x0a\x09\x0a\x03\x08\x90\x03\x0a\x02\x08\x16" [{ it_array = [|300; 42|] }; { it_array = [|400; 22|] }]

type i1ama = i1 arraymsg array [@@deriving show, protobuf]
let test_i1ama ctxt =
  assert_roundtrip show_i1ama i1ama_to_protobuf i1ama_from_protobuf
                   "" [||]
; assert_roundtrip show_i1ama i1ama_to_protobuf i1ama_from_protobuf
                   "\x0a\x05\x0a\x03\x08\xac\x02" [|{ it_array = [|300|] }|]
; assert_roundtrip show_i1ama i1ama_to_protobuf i1ama_from_protobuf
                   "\x0a\x09\x0a\x03\x08\xac\x02\x0a\x02\x08\x2a" [|{ it_array = [|300; 42|] }|]
; assert_roundtrip show_i1ama i1ama_to_protobuf i1ama_from_protobuf
                   "\x0a\x09\x0a\x03\x08\xac\x02\x0a\x02\x08\x2a\x0a\x09\x0a\x03\x08\x90\x03\x0a\x02\x08\x16" [|{ it_array = [|300; 42|] }; { it_array = [|400; 22|] }|]


#ifdef PAPPX
type i1oo = i1 option option [@@deriving show, protobuf]
let test_i1oo ctxt =
  assert_roundtrip show_i1oo i1oo_to_protobuf i1oo_from_protobuf
                   "" None
; assert_roundtrip show_i1oo i1oo_to_protobuf i1oo_from_protobuf
                   "\x0a\x00" (Some None)
; assert_roundtrip show_i1oo i1oo_to_protobuf i1oo_from_protobuf
                   "\x0a\x05\x0a\x03\x08\xac\x02" (Some (Some 300))

type i1ol = i1 option list [@@deriving show, protobuf]
let test_i1ol ctxt =
  assert_roundtrip show_i1ol i1ol_to_protobuf i1ol_from_protobuf
                   "" []
; assert_roundtrip show_i1ol i1ol_to_protobuf i1ol_from_protobuf
                   "\x0a\x00" [None]
; assert_roundtrip show_i1ol i1ol_to_protobuf i1ol_from_protobuf
                   "\x0a\x05\x0a\x03\x08\xac\x02" [(Some 300)]
; assert_roundtrip show_i1ol i1ol_to_protobuf i1ol_from_protobuf
                   "\x0a\x05\x0a\x03\x08\xac\x02\x0a\x04\x0a\x02\x08\x2a" [(Some 300);(Some 42)]

type i1oa = i1 option array [@@deriving show, protobuf]
let test_i1oa ctxt =
  assert_roundtrip show_i1oa i1oa_to_protobuf i1oa_from_protobuf
                   "" [||]
; assert_roundtrip show_i1oa i1oa_to_protobuf i1oa_from_protobuf
                   "\x0a\x00" [|None|]
; assert_roundtrip show_i1oa i1oa_to_protobuf i1oa_from_protobuf
                   "\x0a\x05\x0a\x03\x08\xac\x02" [|(Some 300)|]
; assert_roundtrip show_i1oa i1oa_to_protobuf i1oa_from_protobuf
                   "\x0a\x05\x0a\x03\x08\xac\x02\x0a\x04\x0a\x02\x08\x2a" [|(Some 300);(Some 42)|]


type i1ll = i1 list list [@@deriving show, protobuf]
let test_i1ll ctxt =
  assert_roundtrip show_i1ll i1ll_to_protobuf i1ll_from_protobuf
                   "" []
; assert_roundtrip show_i1ll i1ll_to_protobuf i1ll_from_protobuf
                   "\x0a\x05\x0a\x03\x08\xac\x02" [[300]]
; assert_roundtrip show_i1ll i1ll_to_protobuf i1ll_from_protobuf
                   "\x0a\x09\x0a\x03\x08\xac\x02\x0a\x02\x08\x2a" [[300; 42]]
; assert_roundtrip show_i1ll i1ll_to_protobuf i1ll_from_protobuf
                   "\x0a\x09\x0a\x03\x08\xac\x02\x0a\x02\x08\x2a\x0a\x09\x0a\x03\x08\x90\x03\x0a\x02\x08\x16"
                   [[300; 42];[400; 22]]

type i1lo = i1 list option [@@deriving show, protobuf]
let test_i1lo ctxt =
  assert_roundtrip show_i1lo i1lo_to_protobuf i1lo_from_protobuf
                   "" None
; assert_roundtrip show_i1lo i1lo_to_protobuf i1lo_from_protobuf
                   "\x0a\x05\x0a\x03\x08\xac\x02" (Some [300])
; assert_roundtrip show_i1lo i1lo_to_protobuf i1lo_from_protobuf
                   "\x0a\x09\x0a\x03\x08\xac\x02\x0a\x02\x08\x2a" (Some [300; 42])

type i1la = i1 list array [@@deriving show, protobuf]
let test_i1la ctxt =
  assert_roundtrip show_i1la i1la_to_protobuf i1la_from_protobuf
                   "" [||]
; assert_roundtrip show_i1la i1la_to_protobuf i1la_from_protobuf
                   "\x0a\x05\x0a\x03\x08\xac\x02" [|[300]|]
; assert_roundtrip show_i1la i1la_to_protobuf i1la_from_protobuf
                   "\x0a\x09\x0a\x03\x08\xac\x02\x0a\x02\x08\x2a" [|[300; 42]|]
; assert_roundtrip show_i1la i1la_to_protobuf i1la_from_protobuf
                   "\x0a\x09\x0a\x03\x08\xac\x02\x0a\x02\x08\x2a\x0a\x09\x0a\x03\x08\x90\x03\x0a\x02\x08\x16"
                   [|[300; 42];[400; 22]|]

type i1ao = i1 array option [@@deriving show, protobuf]
let test_i1ao ctxt =
  assert_roundtrip show_i1ao i1ao_to_protobuf i1ao_from_protobuf
                   "" None
; assert_roundtrip show_i1ao i1ao_to_protobuf i1ao_from_protobuf
                   "\x0a\x05\x0a\x03\x08\xac\x02" (Some [|300|])
; assert_roundtrip show_i1ao i1ao_to_protobuf i1ao_from_protobuf
                   "\x0a\x09\x0a\x03\x08\xac\x02\x0a\x02\x08\x2a" (Some [|300; 42|])

type i1al = i1 array list [@@deriving show, protobuf]
let test_i1al ctxt =
  assert_roundtrip show_i1al i1al_to_protobuf i1al_from_protobuf
                   "" []
; assert_roundtrip show_i1al i1al_to_protobuf i1al_from_protobuf
                   "\x0a\x05\x0a\x03\x08\xac\x02" [[|300|]]
; assert_roundtrip show_i1al i1al_to_protobuf i1al_from_protobuf
                   "\x0a\x09\x0a\x03\x08\xac\x02\x0a\x02\x08\x2a" [[|300; 42|]]
; assert_roundtrip show_i1al i1al_to_protobuf i1al_from_protobuf
                   "\x0a\x09\x0a\x03\x08\xac\x02\x0a\x02\x08\x2a\x0a\x09\x0a\x03\x08\x90\x03\x0a\x02\x08\x16" [[|300; 42|];[|400; 22|]]

type i1aa = i1 array array [@@deriving show, protobuf]
let test_i1aa ctxt =
  assert_roundtrip show_i1aa i1aa_to_protobuf i1aa_from_protobuf
                   "" [||]
; assert_roundtrip show_i1aa i1aa_to_protobuf i1aa_from_protobuf
                   "\x0a\x05\x0a\x03\x08\xac\x02" [|[|300|]|]
; assert_roundtrip show_i1aa i1aa_to_protobuf i1aa_from_protobuf
                   "\x0a\x09\x0a\x03\x08\xac\x02\x0a\x02\x08\x2a" [|[|300; 42|]|]
; assert_roundtrip show_i1aa i1aa_to_protobuf i1aa_from_protobuf
                   "\x0a\x09\x0a\x03\x08\xac\x02\x0a\x02\x08\x2a\x0a\x09\x0a\x03\x08\x90\x03\x0a\x02\x08\x16" [|[|300; 42|];[|400; 22|]|]


#endif

type v5 =
| V5A of int option [@key 1]
| V5B of string list [@key 2]
| V5C of int array [@key 3]
| V5D [@key 4]
[@@deriving protobuf]
let test_variant_optrep ctxt =
  let printer v5 =
    match v5 with
    | V5A io -> (match io with Some i -> Printf.sprintf "V5A %d" i | None -> "V5A None")
    | V5B sl -> Printf.sprintf "V5B [%s]" (String.concat "; " sl)
    | V5C ia -> Printf.sprintf "V5C [|%s|]" (String.concat "; "
                                              (List.map string_of_int (Array.to_list ia)))
    | V5D -> "V5D"
  in
  assert_roundtrip printer v5_to_protobuf v5_from_protobuf
                   "\x08\x01\x10\x2a" (V5A (Some 42));
  assert_roundtrip printer v5_to_protobuf v5_from_protobuf
                   "\x08\x01" (V5A None);
  assert_roundtrip printer v5_to_protobuf v5_from_protobuf
                   "\x08\x02\x1a\x0242\x1a\x0243" (V5B ["42"; "43"]);
  assert_roundtrip printer v5_to_protobuf v5_from_protobuf
                   "\x08\x02" (V5B []);
  assert_roundtrip printer v5_to_protobuf v5_from_protobuf
                   "\x08\x03\x20\x2a\x20\x2b" (V5C [|42; 43|]);
  assert_roundtrip printer v5_to_protobuf v5_from_protobuf
                   "\x08\x03" (V5C [||])

type r9 = i1 r5 [@@deriving protobuf]
let test_nonpoly ctxt =
  let printer { r5a } = Printf.sprintf "{ r5a = %d }" r5a in
  assert_roundtrip printer r9_to_protobuf r9_from_protobuf
                   "\x0a\x04\x0a\x02\x08\x01" { r5a = 1 }

type d = int [@default 42] [@@deriving protobuf]
let test_default ctxt =
  assert_roundtrip string_of_int d_to_protobuf d_from_protobuf
                   "" 42;
  assert_roundtrip string_of_int d_to_protobuf d_from_protobuf
                   "\x08\x01" 1

type p = int list [@packed] [@@deriving protobuf]
let test_packed ctxt =
  let printer xs = Printf.sprintf "[%s]" (String.concat "; " (List.map string_of_int xs)) in
  assert_roundtrip printer p_to_protobuf p_from_protobuf
                   "" [];
  assert_roundtrip printer p_to_protobuf p_from_protobuf
                   "\x0a\x01\x01" [1];
  assert_roundtrip printer p_to_protobuf p_from_protobuf
                   "\x0a\x03\x01\x02\x03" [1; 2; 3];
  let d = Protobuf.Decoder.of_string "\x0a\x01\x01\x0a\x02\x02\x03" in
  assert_equal ~printer [1; 2; 3] (p_from_protobuf d)

type p' = int array [@packed] [@@deriving protobuf]
let test_packed ctxt =
  let printer xs =
  let xs = Array.to_list xs in
  Printf.sprintf "[%s]" (String.concat "; " (List.map string_of_int xs)) in
  assert_roundtrip printer p'_to_protobuf p'_from_protobuf
                   "" [||];
  assert_roundtrip printer p'_to_protobuf p'_from_protobuf
                   "\x0a\x01\x01" [|1|];
  assert_roundtrip printer p'_to_protobuf p'_from_protobuf
                   "\x0a\x03\x01\x02\x03" [|1; 2; 3|];
  let d = Protobuf.Decoder.of_string "\x0a\x01\x01\x0a\x02\x02\x03" in
  assert_equal ~printer [|1; 2; 3|] (p'_from_protobuf d)

#ifndef PAPPX
let test_errors ctxt =
  (* scalars *)
  let d = Protobuf.Decoder.of_string "" in
  assert_raises Protobuf.Decoder.(Failure (Missing_field (filemod^".s")))
                (fun () -> s_from_protobuf d);
  let d = Protobuf.Decoder.of_string "\x0d\x00\x00\xC0\x3f" in
  assert_raises Protobuf.Decoder.(Failure (Unexpected_payload (filemod^".s", Protobuf.Bits32)))
                (fun () -> s_from_protobuf d);
  (* records *)
  let d = Protobuf.Decoder.of_string "" in
  assert_raises Protobuf.Decoder.(Failure (Missing_field (filemod^".r1.r1b")))
                (fun () -> r1_from_protobuf d);
  (* tuples *)
  let d = Protobuf.Decoder.of_string "\x0a\x00" in
  assert_raises Protobuf.Decoder.(Failure (Missing_field (filemod^".r3.r3a.1")))
                (fun () -> r3_from_protobuf d);
  (* variants *)
  let d = Protobuf.Decoder.of_string "\x08\x03\x18\x1a" in
  assert_raises Protobuf.Decoder.(Failure (Malformed_variant (filemod^".v1")))
                (fun () -> v1_from_protobuf d)

let test_skip ctxt =
  let d = Protobuf.Decoder.of_string "\x15\x00\x00\xC0\x3f" in
  assert_raises Protobuf.Decoder.(Failure (Missing_field (filemod^".s")))
                (fun () -> s_from_protobuf d)

module type Elem = sig
  type t [@@deriving protobuf]
end

module Collection(Elem:Elem) = struct
  type t = Elem.t list [@@deriving protobuf]
end
#endif

let suite = "Test syntax" >::: [
    "test_bool"           >:: test_bool;
    "test_ints"           >:: test_ints;
    "test_uints"          >:: test_uints;
    "test_floats"         >:: test_floats;
    "test_string"         >:: test_string;
    "test_bytes"         >:: test_bytes;
    "test_option"         >:: test_option;
    "test_list"           >:: test_list;
    "test_array"          >:: test_array;
    "test_tuple"          >:: test_tuple;
    "test_tuple'"          >:: test_tuple';
    "test_tup3"          >:: test_tup3;
    "test_ts''"          >:: test_ts'';
    "test_tsts"          >:: test_tsts;
    "test_tsts''"          >:: test_tsts'';
    "test_record"         >:: test_record;
    "test_nested"         >:: test_nested;
    "test_imm_tuple"      >:: test_imm_tuple;
    "test_variant"        >:: test_variant;
#ifndef PAPPX
    "test_variant_bare"   >:: test_variant_bare;
#endif
    "test_tvar"           >:: test_tvar;
    "test_mylist"         >:: test_mylist;
    "test_poly_variant"   >:: test_poly_variant;
    "test_imm_pvariant"   >:: test_imm_pvariant;
#ifndef PAPPX
    "test_pvariant_bare"  >:: test_pvariant_bare;
    "test_imm_pv_bare"    >:: test_imm_pv_bare;
#endif
    "test_optionmsg"      >:: test_optionmsg;
    "test_listmsg"        >:: test_listmsg;
    "test_arraymsg"       >:: test_arraymsg;

    "test_i1om"           >:: test_i1om;
    "test_i1omo"          >:: test_i1omo;
    "test_i1oml"          >:: test_i1oml;
    "test_i1oma"          >:: test_i1oma;

    "test_i1lm"           >:: test_i1lm;
    "test_i1lmo"          >:: test_i1lmo;
    "test_i1lml"          >:: test_i1lml;
    "test_i1lma"          >:: test_i1lma;

    "test_i1am"           >:: test_i1am;
    "test_i1amo"          >:: test_i1amo;
    "test_i1aml"          >:: test_i1aml;
    "test_i1ama"          >:: test_i1ama;
#ifdef PAPPX
    "test_i1ol"           >:: test_i1ol;
    "test_i1oo"           >:: test_i1oo;
    "test_i1oa"           >:: test_i1oa;

    "test_i1ll"           >:: test_i1ll;
    "test_i1lo"           >:: test_i1lo;
    "test_i1la"           >:: test_i1la;

    "test_i1ao"           >:: test_i1ao;
    "test_i1al"           >:: test_i1al;
    "test_i1aa"           >:: test_i1aa;
#endif
    "test_variant_optrep" >:: test_variant_optrep;
    "test_nonpoly"        >:: test_nonpoly;
    "test_default"        >:: test_default;
    "test_packed"         >:: test_packed;
#ifndef PAPPX
    "test_errors"         >:: test_errors;
    "test_skip"           >:: test_skip;
#endif
  ]
let _ =
  if not !Sys.interactive then run_test_tt_main suite

