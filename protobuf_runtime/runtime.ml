(* camlp5r *)
(* runtime.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

module Stdlib = Stdlib ;

module Protobuf = Protobuf ;

module Encode = struct
value required f v encoder =
  f v encoder
;
value optional f v encoder =
  match v with [ None -> () | Some v -> f v encoder ]
;

value list f v encoder =
  List.iter (fun e -> f e encoder) v
;

value array f v encoder =
  let v = Array.to_list v in
  list f v encoder
;

value int__varint = fun ~{key} ~{msg} v encoder ->
((      let _alias = v in
      do { Protobuf.Encoder.key (key, Protobuf.Varint) encoder;
       Protobuf.Encoder.varint (Int64.of_int _alias) encoder}) (*  *)
  [@ocaml.warning "-A";]) ;

value bool__varint = fun ~{key} ~{msg} v encoder ->
      let _alias = v in do {
        Protobuf.Encoder.key (key, Protobuf.Varint) encoder;
        Protobuf.Encoder.varint (if _alias then 1L else 0L) encoder
      }
  [@ocaml.warning "-A";] ;

value int__zigzag ~{key} ~{msg} v encoder =
  (
      let _alias = v in do
      {Protobuf.Encoder.key (key, Protobuf.Varint) encoder;
       Protobuf.Encoder.zigzag (Int64.of_int _alias) encoder}
  [@ocaml.warning "-A";]) ;

value int__bits32 ~{key} ~{msg} v encoder =
  ((
      let _alias = v in
      do {Protobuf.Encoder.key (key, Protobuf.Bits32) encoder;
       Protobuf.Encoder.bits32
         (Protobuf.Encoder.int32_of_int msg _alias)
         encoder})
  [@ocaml.warning "-A";]) ;

value int__bits64 ~{key} ~{msg} v encoder =
  ((
      let _alias = v in
      do {Protobuf.Encoder.key (key, Protobuf.Bits64) encoder;
       Protobuf.Encoder.bits64 (Int64.of_int _alias) encoder})
  [@ocaml.warning "-A";]) ;

value int32__varint ~{key} ~{msg} _value encoder =
  ((
      let _alias = _value in
      do {Protobuf.Encoder.key (key, Protobuf.Varint) encoder;
       Protobuf.Encoder.varint (Int64.of_int32 _alias) encoder})
  [@ocaml.warning "-A";]) ;

(* TODO: There seems to be a bug here with the key *)
value int32__zigzag ~{key} ~{msg} _value encoder =
  ((
      let _alias = _value in
      do {Protobuf.Encoder.key (key, Protobuf.Varint) encoder;
       Protobuf.Encoder.zigzag (Int64.of_int32 _alias) encoder})
  [@ocaml.warning "-A";]) ;

value int32__bits32 ~{key} ~{msg} _value encoder =
  ((
      let _alias = _value in
      do {Protobuf.Encoder.key (key, Protobuf.Bits32) encoder;
       Protobuf.Encoder.bits32 _alias encoder})
  [@ocaml.warning "-A";]) ;

value int32__bits64 ~{key} ~{msg} _value encoder =
  ((
      let _alias = _value in
      do {Protobuf.Encoder.key (key, Protobuf.Bits64) encoder;
       Protobuf.Encoder.bits64 (Int64.of_int32 _alias) encoder})
  [@ocaml.warning "-A";]) ;

value int64__varint ~{key} ~{msg} _value encoder =
  ((
      let _alias = _value in
      do {Protobuf.Encoder.key (key, Protobuf.Varint) encoder;
       Protobuf.Encoder.varint _alias encoder})
  [@ocaml.warning "-A";]) ;

value int64__zigzag ~{key} ~{msg} _value encoder =
  ((
      let _alias = _value in
      do {Protobuf.Encoder.key (key, Protobuf.Varint) encoder;
       Protobuf.Encoder.zigzag _alias encoder})
  [@ocaml.warning "-A";]) ;

value int64__bits32 ~{key} ~{msg} _value encoder =
  ((
      let _alias = _value in
      do {Protobuf.Encoder.key (key, Protobuf.Bits32) encoder;
       Protobuf.Encoder.bits32
         (Protobuf.Encoder.int32_of_int64 msg _alias)
         encoder})
  [@ocaml.warning "-A";]) ;

value int64__bits64 ~{key} ~{msg} _value encoder =
  ((
      let _alias = _value in
      do {Protobuf.Encoder.key (key, Protobuf.Bits64) encoder;
       Protobuf.Encoder.bits64 _alias encoder})
  [@ocaml.warning "-A";]) ;

value uint32__varint ~{key} ~{msg} _value encoder =
  ((
      let _alias = _value in
      do {Protobuf.Encoder.key (key, Protobuf.Varint) encoder;
       Protobuf.Encoder.varint (Int64.of_int32 (Uint32.to_int32 _alias))
         encoder})
  [@ocaml.warning "-A";]) ;

value uint32__zigzag ~{key} ~{msg} _value encoder =
  ((
      let _alias = _value in
      do {Protobuf.Encoder.key (key, Protobuf.Varint) encoder;
       Protobuf.Encoder.zigzag (Int64.of_int32 (Uint32.to_int32 _alias))
         encoder})
  [@ocaml.warning "-A";]) ;

value uint32__bits32 ~{key} ~{msg} _value encoder =
  ((
      let _alias = _value in
      do {Protobuf.Encoder.key (key, Protobuf.Bits32) encoder;
       Protobuf.Encoder.bits32 (Uint32.to_int32 _alias) encoder})
  [@ocaml.warning "-A";]) ;

value uint32__bits64 ~{key} ~{msg} _value encoder =
  ((
      let _alias = _value in
      do {Protobuf.Encoder.key (key, Protobuf.Bits64) encoder;
       Protobuf.Encoder.bits64 (Int64.of_int32 (Uint32.to_int32 _alias))
         encoder})
  [@ocaml.warning "-A";]) ;

value uint64__varint ~{key} ~{msg} _value encoder =
  ((
      let _alias = _value in
      do {Protobuf.Encoder.key (key, Protobuf.Varint) encoder;
       Protobuf.Encoder.varint (Uint64.to_int64 _alias) encoder})
  [@ocaml.warning "-A";]) ;

value uint64__zigzag ~{key} ~{msg} _value encoder =
  ((
      let _alias = _value in
      do {Protobuf.Encoder.key (key, Protobuf.Varint) encoder;
       Protobuf.Encoder.zigzag (Uint64.to_int64 _alias) encoder})
  [@ocaml.warning "-A";]) ;

value uint64__bits32 ~{key} ~{msg} _value encoder =
  ((
      let _alias = _value in
      do {Protobuf.Encoder.key (key, Protobuf.Bits32) encoder;
       Protobuf.Encoder.bits32
         (Protobuf.Encoder.int32_of_int64 msg
            (Uint64.to_int64 _alias)) encoder})
  [@ocaml.warning "-A";]) ;

value uint64__bits64 ~{key} ~{msg} _value encoder =
  ((
      let _alias = _value in
      do {Protobuf.Encoder.key (key, Protobuf.Bits64) encoder;
       Protobuf.Encoder.bits64 (Uint64.to_int64 _alias) encoder})
  [@ocaml.warning "-A";]) ;

value float__bits32 ~{key} ~{msg} _value encoder =
  ((
      let _alias = _value in
      do {Protobuf.Encoder.key (key, Protobuf.Bits32) encoder;
       Protobuf.Encoder.bits32 (Int32.bits_of_float _alias) encoder})
  [@ocaml.warning "-A";]) ;

value float__bits64 ~{key} ~{msg} _value encoder =
  ((
      let _alias = _value in
      do {Protobuf.Encoder.key (key, Protobuf.Bits64) encoder;
       Protobuf.Encoder.bits64 (Int64.bits_of_float _alias) encoder})
  [@ocaml.warning "-A";]) ;

value string__bytes ~{key} ~{msg} _value encoder =
  ((
      let _alias = _value in
      do {Protobuf.Encoder.key (key, Protobuf.Bytes) encoder;
       Protobuf.Encoder.bytes (Bytes.of_string _alias) encoder})
  [@ocaml.warning "-A";]) ;

value bytes__bytes ~{key} ~{msg} _value encoder =
  ((
      let _alias = _value in
      do {Protobuf.Encoder.key (key, Protobuf.Bytes) encoder;
       Protobuf.Encoder.bytes _alias encoder})
  [@ocaml.warning "-A";]) ;

end ;

module Decode = struct

value required ~{msg} f decoder =
  match f decoder with [
    None ->
      raise
        (let open Protobuf.Decoder in
            Failure (Missing_field msg))
  | Some v -> v ]
;

value optional ~{msg} f decoder =
   f decoder ;

value last ~{msg} f decoder =
  let rec drec prev =
  match f decoder with [
    None -> prev
  | Some v -> drec (Some v)
  ] in drec None
;

value required_last ~{msg} f decoder =
  required ~{msg=msg} (last ~{msg} f) decoder
;

value optional_last = last ;

value list ~{msg} f decoder =
  let rec drec acc =
  match f decoder with [
    None -> List.rev acc
  | Some v -> drec [v :: acc]
  ] in drec []
;

value array ~{msg} f decoder =
  let l = list f ~{msg} decoder in
  Array.of_list l
;

type converter_t 'a 'b = {
  kind : Protobuf.payload_kind
; decodef : Protobuf.Decoder.t -> 'a
; convertf : string -> 'a -> 'b
} ;

value decode0 c ~{msg} kind (decoder : Protobuf.Decoder.t) =
  let open Protobuf.Decoder in
  if c.kind = kind then
    Some (c.convertf msg (c.decodef decoder))
  else
    raise (Failure (Unexpected_payload msg kind))
;

value decode1 c ~{wantkey} ~{msg} (decoder : Protobuf.Decoder.t) =
  let open Protobuf.Decoder in
  match key decoder with [
    Some (gotkey, gotkind) when wantkey = gotkey ->
      decode0 c ~{msg=msg} gotkind decoder
  | None -> None ]
;

value decoding_key f ~{wantkey} ~{msg} decoder =
  let open Protobuf.Decoder in
  match key decoder with [
    Some (gotkey, gotkind) when wantkey = gotkey ->
      f ~{msg=msg} gotkind decoder
  | None -> None ]
;

value int__varint =
  { kind = Protobuf.Varint ; convertf=Protobuf.Decoder.int_of_int64; decodef=Protobuf.Decoder.varint } ;

value bool__variant = 
  { kind = Protobuf.Varint ; convertf=Protobuf.Decoder.bool_of_int64; decodef=Protobuf.Decoder.varint } ;

value int__zigzag =
  {kind=Protobuf.Varint; convertf=Protobuf.Decoder.int_of_int64; decodef=Protobuf.Decoder.zigzag } ;

value int__bits32 =
  {kind=Protobuf.Bits32; convertf=Protobuf.Decoder.int_of_int32; decodef=Protobuf.Decoder.bits32} ;

value int__bits64 = 
  {kind=Protobuf.Bits64 ; convertf=Protobuf.Decoder.int_of_int64; decodef=Protobuf.Decoder.bits64} ;

value int32__varint = 
  {kind=Protobuf.Varint ; convertf=Protobuf.Decoder.int32_of_int64; decodef=Protobuf.Decoder.varint} ;

value int32__zigzag = 
  {kind=Protobuf.Varint ; convertf=Protobuf.Decoder.int32_of_int64; decodef=Protobuf.Decoder.zigzag} ;

value id x = x ;
value forget1 f x y = f y ;

value int32__bits32 = 
  {kind=Protobuf.Bits32 ; convertf=forget1 id; decodef=Protobuf.Decoder.bits32} ;

value int32__bits64 = 
  {kind=Protobuf.Bits64 ; convertf=Protobuf.Decoder.int32_of_int64; decodef=Protobuf.Decoder.bits64} ;

value int64__varint = 
  {kind=Protobuf.Varint ; convertf=forget1 id; decodef=Protobuf.Decoder.varint} ;

value int64__zigzag = 
  {kind=Protobuf.Varint ; convertf=forget1 id; decodef=Protobuf.Decoder.zigzag} ;

value int64__bits32 = 
  {kind=Protobuf.Bits32 ; convertf=forget1 Int64.of_int32; decodef=Protobuf.Decoder.bits32} ;

value int64__bits64 = 
  {kind=Protobuf.Bits64 ; convertf=forget1 id; decodef=Protobuf.Decoder.bits64} ;

value uint32_of_int64 msg n =
  Uint32.of_int32 (Protobuf.Decoder.int32_of_int64 msg n)
;

value uint32__varint = 
  {kind=Protobuf.Varint ; convertf=uint32_of_int64; decodef=Protobuf.Decoder.varint} ;

value uint32__zigzag = 
  {kind=Protobuf.Varint ; convertf=uint32_of_int64; decodef=Protobuf.Decoder.zigzag} ;

value uint32__bits32 = 
  {kind=Protobuf.Bits32 ; convertf=forget1 Uint32.of_int32; decodef=Protobuf.Decoder.bits32} ;

value uint32_of_int64 msg n =
  Uint32.of_int32 (Protobuf.Decoder.int32_of_int64 msg n) ;

value uint32__bits64 = 
  {kind=Protobuf.Bits64 ; convertf=uint32_of_int64; decodef=Protobuf.Decoder.bits64} ;

value uint64__varint = 
  {kind=Protobuf.Varint ; convertf=forget1 Uint64.of_int64; decodef=Protobuf.Decoder.varint} ;

value uint64__zigzag = 
  {kind=Protobuf.Varint ; convertf=forget1 Uint64.of_int64; decodef=Protobuf.Decoder.zigzag} ;

value uint64__bits32 = 
  {kind=Protobuf.Bits32 ; convertf=forget1 Uint64.of_int32; decodef=Protobuf.Decoder.bits32} ;

value uint64__bits64 = 
  {kind=Protobuf.Bits64 ; convertf=forget1 Uint64.of_int64; decodef=Protobuf.Decoder.bits64} ;

value float__bits32 = 
  {kind=Protobuf.Bits32 ; convertf=forget1 Int32.float_of_bits; decodef=Protobuf.Decoder.bits32} ;

value float__bits64 = 
  {kind=Protobuf.Bits64 ; convertf=forget1 Int64.float_of_bits; decodef=Protobuf.Decoder.bits64} ;

value string__bytes = 
  {kind=Protobuf.Bytes ; convertf=forget1 Bytes.to_string; decodef=Protobuf.Decoder.bytes} ;

value bytes__bytes = 
  {kind=Protobuf.Bytes ; convertf=forget1 id; decodef=Protobuf.Decoder.bytes} ;

end ;
