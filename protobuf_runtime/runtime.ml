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

value int__varint ~{wantkey} ~{msg} decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (gotkey, Protobuf.Varint) when wantkey = gotkey ->
            do {_alias.val :=
               (Some
                  (Protobuf.Decoder.int_of_int64 msg
                     (Protobuf.Decoder.varint decoder))) }
        | Some (gotkey, kind) when wantkey = gotkey ->
            raise
              (let open Protobuf.Decoder in
                 Failure (Unexpected_payload msg kind))
        | None -> () ] in do {
      read (); _alias.val })
  [@ocaml.warning "-A";]) ;

value bool__variant ~{wantkey} ~{msg} decoder =
((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (gotkey, Protobuf.Varint) when wantkey = gotkey -> do {
            _alias.val :=
               (Some
                  (Protobuf.Decoder.bool_of_int64 msg
                     (Protobuf.Decoder.varint decoder))) }
        | Some (gotkey, kind) when wantkey = gotkey ->
            raise
              (let open Protobuf.Decoder in
                 Failure (Unexpected_payload msg kind))
        | None -> () ] in do {
      read (); _alias.val }
      )
  [@ocaml.warning "-A";]) ;

value int__zigzag ~{wantkey} ~{msg} decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
         Some (gotkey, Protobuf.Varint) when wantkey = gotkey -> do
            {_alias.val :=
               (Some
                  (Protobuf.Decoder.int_of_int64 msg
                     (Protobuf.Decoder.zigzag decoder))) }
        | Some (gotkey, kind) when wantkey = gotkey ->
            raise
              (let open Protobuf.Decoder in
                 Failure (Unexpected_payload msg kind))
        | None -> () ] in do {
      read (); _alias.val })
  [@ocaml.warning "-A";]) ;

value int__bits32 ~{wantkey} ~{msg} decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
         Some (gotkey, Protobuf.Bits32) when wantkey = gotkey ->
            do {_alias.val :=
               (Some
                  (Protobuf.Decoder.int_of_int32 msg
                     (Protobuf.Decoder.bits32 decoder))) }
        | Some (gotkey, kind) when wantkey = gotkey ->
            raise
              (let open Protobuf.Decoder in
                 Failure (Unexpected_payload msg kind))
        | None -> () ] in
      do { read (); _alias.val })
  [@ocaml.warning "-A";]) ;

value int__bits64 ~{wantkey} ~{msg} decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (gotkey, Protobuf.Bits64) when wantkey = gotkey ->
            do {_alias.val :=
               (Some
                  (Protobuf.Decoder.int_of_int64 msg
                     (Protobuf.Decoder.bits64 decoder))) }
        | Some (gotkey, kind) when wantkey = gotkey ->
            raise
              (let open Protobuf.Decoder in
                 Failure (Unexpected_payload msg kind))
        | None -> () ] in
      do {read (); _alias.val })
  [@ocaml.warning "-A";]) ;

value int32__varint ~{wantkey} ~{msg} decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
         Some (gotkey, Protobuf.Varint) when wantkey = gotkey ->
            do {_alias.val :=
               (Some
                  (Protobuf.Decoder.int32_of_int64 msg
                     (Protobuf.Decoder.varint decoder))) }
        | Some (gotkey, kind) when wantkey = gotkey ->
            raise
              (let open Protobuf.Decoder in
                 Failure
                   (Unexpected_payload "Test_syntax.ml.ppx.il1" kind))
        | None -> () ] in
      do { read (); _alias.val })
  [@ocaml.warning "-A";]) ;

value int32__zigzag ~{wantkey} ~{msg} decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (gotkey, Protobuf.Varint) when wantkey = gotkey ->
            do {_alias.val :=
               (Some
                  (Protobuf.Decoder.int32_of_int64 msg
                     (Protobuf.Decoder.zigzag decoder))) }
        | Some (gotkey, kind) when wantkey = gotkey ->
            raise
              (let open Protobuf.Decoder in
                 Failure
                   (Unexpected_payload msg kind))
        | None -> () ] in
      do { read (); _alias.val })
  [@ocaml.warning "-A";]);

value int32__bits32 ~{wantkey} ~{msg} decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (gotkey, Protobuf.Bits32) when wantkey = gotkey ->
            do {_alias.val := (Some (Protobuf.Decoder.bits32 decoder)) }
        | Some (gotkey, kind) when wantkey = gotkey ->
            raise
              (let open Protobuf.Decoder in
                 Failure
                   (Unexpected_payload msg kind))
        | None -> () ] in
      do { read (); _alias.val })
  [@ocaml.warning "-A";]) ;

value int32__bits64 ~{wantkey} ~{msg} decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (gotkey, Protobuf.Bits64) when wantkey = gotkey ->
            do {_alias.val :=
               (Some
                  (Protobuf.Decoder.int32_of_int64 msg
                     (Protobuf.Decoder.bits64 decoder))) }
        | Some (gotkey, kind) when wantkey = gotkey ->
            raise
              (let open Protobuf.Decoder in
                 Failure
                   (Unexpected_payload msg kind))
        | None -> () ] in
      do { read (); _alias.val })
  [@ocaml.warning "-A";]) ;

value int64__varint ~{wantkey} ~{msg} decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (gotkey, Protobuf.Varint) when wantkey = gotkey ->
            do {_alias.val := (Some (Protobuf.Decoder.varint decoder)) }
        | Some (gotkey, kind) when wantkey = gotkey ->
            raise
              (let open Protobuf.Decoder in
                 Failure
                   (Unexpected_payload msg kind))
        | None -> () ] in
      do { read (); _alias.val })
  [@ocaml.warning "-A";]) ;

value int64__zigzag ~{wantkey} ~{msg} decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (gotkey, Protobuf.Varint) when wantkey = gotkey ->
            do {_alias.val := (Some (Protobuf.Decoder.zigzag decoder)) }
        | Some (gotkey, kind) when wantkey = gotkey ->
            raise
              (let open Protobuf.Decoder in
                 Failure
                   (Unexpected_payload msg kind))
        | None -> () ] in
      do { read (); _alias.val })
  [@ocaml.warning "-A";]) ;

value int64__bits32 ~{wantkey} ~{msg} decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (gotkey, Protobuf.Bits32) when wantkey = gotkey ->
            do {_alias.val :=
               (Some (Int64.of_int32 (Protobuf.Decoder.bits32 decoder))) }
        | Some (gotkey, kind) when wantkey = gotkey ->
            raise
              (let open Protobuf.Decoder in
                 Failure
                   (Unexpected_payload msg kind))
        | None -> () ] in
      do { read (); _alias.val })
  [@ocaml.warning "-A";]) ;

value int64__bits64 ~{wantkey} ~{msg} decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (gotkey, Protobuf.Bits64) when wantkey = gotkey ->
            do {_alias.val := (Some (Protobuf.Decoder.bits64 decoder)) }
        | Some (gotkey, kind) when wantkey = gotkey ->
            raise
              (let open Protobuf.Decoder in
                 Failure
                   (Unexpected_payload msg kind))
        | None -> () ] in
      do { read (); _alias.val })
  [@ocaml.warning "-A";]) ;

value uint32__varint ~{wantkey} ~{msg} decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (gotkey, Protobuf.Varint) when wantkey = gotkey ->
            do {_alias.val :=
               (Some
                  (Uint32.of_int32
                     (Protobuf.Decoder.int32_of_int64
                        msg
                        (Protobuf.Decoder.varint decoder)))) }
        | Some (gotkey, kind) when wantkey = gotkey ->
            raise
              (let open Protobuf.Decoder in
                 Failure
                   (Unexpected_payload msg kind))
        | None -> () ] in
      do { read (); _alias.val })
  [@ocaml.warning "-A";]) ;

value uint32__zigzag ~{wantkey} ~{msg} decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (gotkey, Protobuf.Varint) when wantkey = gotkey ->
            do {_alias.val :=
               (Some
                  (Uint32.of_int32
                     (Protobuf.Decoder.int32_of_int64
                        msg
                        (Protobuf.Decoder.zigzag decoder)))) }
        | Some (gotkey, kind) when wantkey = gotkey ->
            raise
              (let open Protobuf.Decoder in
                 Failure
                   (Unexpected_payload msg kind))
        | None -> () ] in
      do { read (); _alias.val })
  [@ocaml.warning "-A";]) ;

value uint32__bits32 ~{wantkey} ~{msg} decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (gotkey, Protobuf.Bits32) when wantkey = gotkey ->
            do {_alias.val :=
               (Some (Uint32.of_int32 (Protobuf.Decoder.bits32 decoder))) }
        | Some (gotkey, kind) when wantkey = gotkey ->
            raise
              (let open Protobuf.Decoder in
                 Failure
                   (Unexpected_payload msg kind))
        | None -> () ] in
      do { read (); _alias.val })
  [@ocaml.warning "-A";]) ;

value uint32__bits64 ~{wantkey} ~{msg} decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (gotkey, Protobuf.Bits64) when wantkey = gotkey ->
            do {_alias.val :=
               (Some
                  (Uint32.of_int32
                     (Protobuf.Decoder.int32_of_int64
                        msg
                        (Protobuf.Decoder.bits64 decoder)))) }
        | Some (gotkey, kind) when wantkey = gotkey ->
            raise
              (let open Protobuf.Decoder in
                 Failure
                   (Unexpected_payload msg kind))
        | None -> () ] in
      do { read (); _alias.val })
  [@ocaml.warning "-A";]) ;

value uint64__varint ~{wantkey} ~{msg} decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (gotkey, Protobuf.Varint) when wantkey = gotkey ->
            do {_alias.val :=
               (Some (Uint64.of_int64 (Protobuf.Decoder.varint decoder))) }
        | Some (gotkey, kind) when wantkey = gotkey ->
            raise
              (let open Protobuf.Decoder in
                 Failure
                   (Unexpected_payload msg kind))
        | None -> () ] in
      do { read (); _alias.val })
  [@ocaml.warning "-A";]) ;

value uint64__zigzag ~{wantkey} ~{msg} decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (gotkey, Protobuf.Varint) when wantkey = gotkey ->
            do {_alias.val :=
               (Some (Uint64.of_int64 (Protobuf.Decoder.zigzag decoder))) }
        | Some (gotkey, kind) when wantkey = gotkey ->
            raise
              (let open Protobuf.Decoder in
                 Failure
                   (Unexpected_payload msg kind))
        | None -> () ] in
      do { read (); _alias.val })
  [@ocaml.warning "-A";]) ;

value uint64__bits32 ~{wantkey} ~{msg} decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (gotkey, Protobuf.Bits32) when wantkey = gotkey ->
            do {_alias.val :=
               (Some (Uint64.of_int32 (Protobuf.Decoder.bits32 decoder))) }
        | Some (gotkey, kind) when wantkey = gotkey ->
            raise
              (let open Protobuf.Decoder in
                 Failure
                   (Unexpected_payload msg kind))
        | None -> () ] in
      do { read (); _alias.val })
  [@ocaml.warning "-A";]) ;

value uint64__bits64 ~{wantkey} ~{msg} decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (gotkey, Protobuf.Bits64) when wantkey = gotkey ->
            do {_alias.val :=
               (Some (Uint64.of_int64 (Protobuf.Decoder.bits64 decoder))) }
        | Some (gotkey, kind) when wantkey = gotkey ->
            raise
              (let open Protobuf.Decoder in
                 Failure
                   (Unexpected_payload msg kind))
        | None -> () ] in
      do { read (); _alias.val })
  [@ocaml.warning "-A";]) ;

value float__bits32 ~{wantkey} ~{msg} decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (gotkey, Protobuf.Bits32) when wantkey = gotkey ->
            do {_alias.val :=
               (Some (Int32.float_of_bits (Protobuf.Decoder.bits32 decoder))) }
        | Some (gotkey, kind) when wantkey = gotkey ->
            raise
              (let open Protobuf.Decoder in
                 Failure (Unexpected_payload msg kind))
        | None -> () ] in
      do { read (); _alias.val })
  [@ocaml.warning "-A";]);

value float__bits64 ~{wantkey} ~{msg} decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (gotkey, Protobuf.Bits64) when wantkey = gotkey ->
            do {_alias.val :=
               (Some (Int64.float_of_bits (Protobuf.Decoder.bits64 decoder))) }
        | Some (gotkey, kind) when wantkey = gotkey ->
            raise
              (let open Protobuf.Decoder in
                 Failure (Unexpected_payload msg kind))
        | None -> () ] in
      do { read (); _alias.val })
  [@ocaml.warning "-A";]) ;

value string__bytes ~{wantkey} ~{msg} decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (gotkey, Protobuf.Bytes) when wantkey = gotkey ->
            do {_alias.val :=
               (Some (Bytes.to_string (Protobuf.Decoder.bytes decoder))) }
        | Some (gotkey, kind) when wantkey = gotkey ->
            raise
              (let open Protobuf.Decoder in
                 Failure (Unexpected_payload msg kind))
        | None -> () ] in
      do { read (); _alias.val })
  [@ocaml.warning "-A";]) ;

value bytes__bytes ~{wantkey} ~{msg} decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (gotkey, Protobuf.Bytes) when wantkey = gotkey ->
            do {_alias.val := (Some (Protobuf.Decoder.bytes decoder)) }
        | Some (gotkey, kind) when wantkey = gotkey ->
            raise
              (let open Protobuf.Decoder in
                 Failure (Unexpected_payload msg kind))
        | None -> () ] in
      do { read (); _alias.val })
  [@ocaml.warning "-A";]) ;

end ;
