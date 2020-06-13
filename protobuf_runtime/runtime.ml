(* camlp5r *)
(* runtime.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

module Stdlib = Stdlib ;

module Protobuf = Protobuf ;

value id x = x ;
value forget1 f x y = f y ;

value int64_of_uint32 n = (Int64.of_int32 (Uint32.to_int32 n)) ;
value int32_of_uint64 msg n = (Protobuf.Encoder.int32_of_int64 msg (Uint64.to_int64 n)) ;
value uint32_of_int64 msg n = Uint32.of_int32 (Protobuf.Decoder.int32_of_int64 msg n) ;

module Encode = struct
value required f v encoder =
  f v encoder
;
value required_default dflt f v encoder =
  if dflt = v then () else
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

type converter_t 'a 'b = {
  kind : Protobuf.payload_kind
; encodef : 'b -> Protobuf.Encoder.t -> unit
; convertf : string -> 'a -> 'b
} ;

value encode0 c ~{key} ~{msg} v encoder =
  do { Protobuf.Encoder.key (key, c.kind) encoder;
       c.encodef (c.convertf msg v) encoder } ;

value list_encode_packed ~{key} ~{msg} c v encoder =
  if v = [] then () else do {
    Protobuf.Encoder.key (key, Protobuf.Bytes) encoder;
    Protobuf.Encoder.nested (fun encoder ->
      List.iter
        (fun v -> c.encodef (c.convertf msg v) encoder) v) encoder
  }
;

value int64_of_bool = fun [ True -> 1L | False -> 0L ] ;

value int__varint = { kind = Protobuf.Varint ; convertf = forget1 Int64.of_int ; encodef = Protobuf.Encoder.varint } ;

value bool__varint = { kind = Protobuf.Varint ; convertf = forget1 int64_of_bool ; encodef = Protobuf.Encoder.varint } ;

value int__zigzag = { kind = Protobuf.Varint ; convertf = forget1 Int64.of_int ; encodef = Protobuf.Encoder.zigzag } ;

value int__bits32 = { kind = Protobuf.Bits32 ; convertf = Protobuf.Encoder.int32_of_int ; encodef = Protobuf.Encoder.bits32 } ;

value int__bits64 = { kind = Protobuf.Bits64 ; convertf = forget1 Int64.of_int ; encodef = Protobuf.Encoder.bits64 } ;

value int32__varint = { kind = Protobuf.Varint ; convertf = forget1 Int64.of_int32 ; encodef = Protobuf.Encoder.varint } ;

value int32__zigzag = { kind = Protobuf.Varint ; convertf = forget1 Int64.of_int32 ; encodef = Protobuf.Encoder.zigzag } ;

value int32__bits32 = { kind = Protobuf.Bits32 ; convertf = forget1 id ; encodef = Protobuf.Encoder.bits32 } ;

value int32__bits64 = { kind = Protobuf.Bits64 ; convertf = forget1 Int64.of_int32 ; encodef = Protobuf.Encoder.bits64 } ;

value int64__varint = { kind = Protobuf.Varint ; convertf = forget1 id ; encodef = Protobuf.Encoder.varint } ;

value int64__zigzag = { kind = Protobuf.Varint ; convertf = forget1 id ; encodef = Protobuf.Encoder.zigzag } ;

value int64__bits32 = { kind = Protobuf.Bits32 ; convertf = Protobuf.Encoder.int32_of_int64 ; encodef = Protobuf.Encoder.bits32 } ;

value int64__bits64 = { kind = Protobuf.Bits64 ; convertf = forget1 id ; encodef = Protobuf.Encoder.bits64 } ;

value uint32__varint = { kind = Protobuf.Varint ; convertf = forget1 int64_of_uint32 ; encodef = Protobuf.Encoder.varint } ;

value uint32__zigzag = { kind = Protobuf.Varint ; convertf = forget1 int64_of_uint32 ; encodef = Protobuf.Encoder.zigzag } ;

value uint32__bits64 = { kind = Protobuf.Bits64 ; convertf = forget1 int64_of_uint32 ; encodef = Protobuf.Encoder.bits64 } ;

value uint32__bits32 = { kind = Protobuf.Bits32 ; convertf = forget1 Uint32.to_int32 ; encodef = Protobuf.Encoder.bits32 } ;

value uint64__varint = { kind = Protobuf.Varint ; convertf = forget1 Uint64.to_int64 ; encodef = Protobuf.Encoder.varint } ;

value uint64__zigzag = { kind = Protobuf.Varint ; convertf = forget1 Uint64.to_int64 ; encodef = Protobuf.Encoder.zigzag } ;

value uint64__bits32 = { kind = Protobuf.Bits32 ; convertf = int32_of_uint64 ; encodef = Protobuf.Encoder.bits32 } ;

value uint64__bits64 = { kind = Protobuf.Bits64 ; convertf = forget1 Uint64.to_int64 ; encodef = Protobuf.Encoder.bits64 } ;

value float__bits32 = { kind = Protobuf.Bits32 ; convertf = forget1 Int32.bits_of_float ; encodef = Protobuf.Encoder.bits32 } ;

value float__bits64 = { kind = Protobuf.Bits64 ; convertf = forget1 Int64.bits_of_float ; encodef = Protobuf.Encoder.bits64 } ;

value string__bytes = { kind = Protobuf.Bytes ; convertf = forget1 Bytes.of_string ; encodef = Protobuf.Encoder.bytes } ;
value bytes__bytes = { kind = Protobuf.Bytes ; convertf = forget1 id ; encodef = Protobuf.Encoder.bytes } ;

end ;

module Decode = struct
(*
value decoding_key f ~{wantkey} ~{msg} decoder =
  let open Protobuf.Decoder in
  match key decoder with [
    Some (gotkey, gotkind) when wantkey = gotkey ->
      Some (f ~{msg=msg} gotkind decoder)
  | None -> None ]
;

value required f ~{msg} ~{wantkey} decoder =
  match decoding_key f ~{msg=msg} ~{wantkey=wantkey} decoder with [
    None ->
      raise
        (let open Protobuf.Decoder in
            Failure (Missing_field msg))
  | Some v -> v ]
;

value optional f ~{msg} ~{wantkey} decoder =
   decoding_key f ~{msg=msg} ~{wantkey=wantkey} decoder ;

value last f ~{msg} ~{wantkey} decoder =
  let rec drec prev =
  match decoding_key f ~{msg} ~{wantkey} decoder with [
    None -> prev
  | Some v -> drec (Some v)
  ] in drec None
;

value required_last f ~{msg} ~{wantkey} decoder =
  match last f ~{msg=msg} ~{wantkey=wantkey} decoder with [
    Some v -> v
  | None -> 
      raise
        (let open Protobuf.Decoder in
            Failure (Missing_field msg))
  ]
;

value optional_last = last ;

value list ~{msg} ~{wantkey} f decoder =
  let rec drec acc =
  match decoding_key f ~{msg} ~{wantkey} decoder with [
    None -> List.rev acc
  | Some v -> drec [v :: acc]
  ] in drec []
;

value array ~{msg} ~{wantkey} f decoder =
  let l = list f ~{msg} ~{wantkey} decoder in
  Array.of_list l
;
*)
type converter_t 'a 'b = {
  kind : Protobuf.payload_kind
; decodef : Protobuf.Decoder.t -> 'a
; convertf : string -> 'a -> 'b
} ;

value decode0 c ~{msg} (decoder : Protobuf.Decoder.t) =
  let open Protobuf.Decoder in
  c.convertf msg (c.decodef decoder)
;
(*
value list_decode_packed ~{msg} ~{key} c decoder =
  let rec drec acc =
    match Protobuf.Decoder.key decoder with [
      Some (gotkey, gotkind) when key = gotkey && c.kind = gotkind ->
        let v = decode0 c ~{msg=msg} decoder in
        drec [ v :: acc ]
    | Some (gotkey, Protobuf.Bytes) when key = gotkey ->
        let decoder = Protobuf.Decoder.nested decoder in
        let rec nrec acc =
          if Protobuf.Decoder.at_end decoder then
            drec acc
          else
            let v = decode0 c ~{msg=msg} decoder in
            nrec [ v :: acc ]
        in nrec acc
    | Some (gotkey, gotkind) when key = gotkey -> 
        raise (let open Protobuf.Decoder in
         Failure (Unexpected_payload msg gotkind))

    | Some (_, kind) -> do { Protobuf.Decoder.skip decoder kind; drec acc }
    | None -> List.rev acc ]
  in drec []
;
*)
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

value uint32__varint = 
  {kind=Protobuf.Varint ; convertf=uint32_of_int64; decodef=Protobuf.Decoder.varint} ;

value uint32__zigzag = 
  {kind=Protobuf.Varint ; convertf=uint32_of_int64; decodef=Protobuf.Decoder.zigzag} ;

value uint32__bits32 = 
  {kind=Protobuf.Bits32 ; convertf=forget1 Uint32.of_int32; decodef=Protobuf.Decoder.bits32} ;

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
