(* camlp5r *)
(* runtime.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

module Stdlib = Stdlib ;

module Protobuf = Protobuf ;

module Encode = struct
value int__varint = fun key v encoder ->
((      let _alias = v in
      do { Protobuf.Encoder.key (key, Protobuf.Varint) encoder;
       Protobuf.Encoder.varint (Int64.of_int _alias) encoder}) (*  *)
  [@ocaml.warning "-A";]) ;

value bool__varint = fun key v encoder ->
      let _alias = v in do {
        Protobuf.Encoder.key (key, Protobuf.Varint) encoder;
        Protobuf.Encoder.varint (if _alias then 1L else 0L) encoder
      }
  [@ocaml.warning "-A";] ;

value int__zigzag key v encoder =
  (
      let _alias = v in do
      {Protobuf.Encoder.key (key, Protobuf.Varint) encoder;
       Protobuf.Encoder.zigzag (Int64.of_int _alias) encoder}
  [@ocaml.warning "-A";]) ;

value int__bits32 msg key v encoder =
  ((
      let _alias = v in
      do {Protobuf.Encoder.key (key, Protobuf.Bits32) encoder;
       Protobuf.Encoder.bits32
         (Protobuf.Encoder.int32_of_int msg _alias)
         encoder})
  [@ocaml.warning "-A";]) ;

value int__bits64 key v encoder =
  ((
      let _alias = v in
      do {Protobuf.Encoder.key (key, Protobuf.Bits64) encoder;
       Protobuf.Encoder.bits64 (Int64.of_int _alias) encoder})
  [@ocaml.warning "-A";]) ;

value int32__varint key _value encoder =
  ((
      let _alias = _value in
      do {Protobuf.Encoder.key (key, Protobuf.Varint) encoder;
       Protobuf.Encoder.varint (Int64.of_int32 _alias) encoder})
  [@ocaml.warning "-A";]) ;

(* TODO: There seems to be a bug here with the key *)
value int32__zigzag key _value encoder =
  ((
      let _alias = _value in
      do {Protobuf.Encoder.key (key, Protobuf.Varint) encoder;
       Protobuf.Encoder.zigzag (Int64.of_int32 _alias) encoder})
  [@ocaml.warning "-A";]) ;

value int32__bits32 key _value encoder =
  ((
      let _alias = _value in
      do {Protobuf.Encoder.key (key, Protobuf.Bits32) encoder;
       Protobuf.Encoder.bits32 _alias encoder})
  [@ocaml.warning "-A";]) ;

value int32__bits64 key _value encoder =
  ((
      let _alias = _value in
      do {Protobuf.Encoder.key (key, Protobuf.Bits64) encoder;
       Protobuf.Encoder.bits64 (Int64.of_int32 _alias) encoder})
  [@ocaml.warning "-A";]) ;

value int64__varint key _value encoder =
  ((
      let _alias = _value in
      do {Protobuf.Encoder.key (key, Protobuf.Varint) encoder;
       Protobuf.Encoder.varint _alias encoder})
  [@ocaml.warning "-A";]) ;

value int64__zigzag key _value encoder =
  ((
      let _alias = _value in
      do {Protobuf.Encoder.key (key, Protobuf.Varint) encoder;
       Protobuf.Encoder.zigzag _alias encoder})
  [@ocaml.warning "-A";]) ;

value int64__bits32 msg key _value encoder =
  ((
      let _alias = _value in
      do {Protobuf.Encoder.key (key, Protobuf.Bits32) encoder;
       Protobuf.Encoder.bits32
         (Protobuf.Encoder.int32_of_int64 msg _alias)
         encoder})
  [@ocaml.warning "-A";]) ;

value int64__bits64 key _value encoder =
  ((
      let _alias = _value in
      do {Protobuf.Encoder.key (key, Protobuf.Bits64) encoder;
       Protobuf.Encoder.bits64 _alias encoder})
  [@ocaml.warning "-A";]) ;

value uint32__varint key _value encoder =
  ((
      let _alias = _value in
      do {Protobuf.Encoder.key (key, Protobuf.Varint) encoder;
       Protobuf.Encoder.varint (Int64.of_int32 (Uint32.to_int32 _alias))
         encoder})
  [@ocaml.warning "-A";]) ;

value uint32__zigzag key _value encoder =
  ((
      let _alias = _value in
      do {Protobuf.Encoder.key (key, Protobuf.Varint) encoder;
       Protobuf.Encoder.zigzag (Int64.of_int32 (Uint32.to_int32 _alias))
         encoder})
  [@ocaml.warning "-A";]) ;

value uint32__bits32 key _value encoder =
  ((
      let _alias = _value in
      do {Protobuf.Encoder.key (key, Protobuf.Bits32) encoder;
       Protobuf.Encoder.bits32 (Uint32.to_int32 _alias) encoder})
  [@ocaml.warning "-A";]) ;

value uint32__bits64 key _value encoder =
  ((
      let _alias = _value in
      do {Protobuf.Encoder.key (key, Protobuf.Bits64) encoder;
       Protobuf.Encoder.bits64 (Int64.of_int32 (Uint32.to_int32 _alias))
         encoder})
  [@ocaml.warning "-A";]) ;

value uint64__varint key _value encoder =
  ((
      let _alias = _value in
      do {Protobuf.Encoder.key (key, Protobuf.Varint) encoder;
       Protobuf.Encoder.varint (Uint64.to_int64 _alias) encoder})
  [@ocaml.warning "-A";]) ;

value uint64__zigzag key _value encoder =
  ((
      let _alias = _value in
      do {Protobuf.Encoder.key (key, Protobuf.Varint) encoder;
       Protobuf.Encoder.zigzag (Uint64.to_int64 _alias) encoder})
  [@ocaml.warning "-A";]) ;

value uint64__bits32 msg key _value encoder =
  ((
      let _alias = _value in
      do {Protobuf.Encoder.key (key, Protobuf.Bits32) encoder;
       Protobuf.Encoder.bits32
         (Protobuf.Encoder.int32_of_int64 msg
            (Uint64.to_int64 _alias)) encoder})
  [@ocaml.warning "-A";]) ;

value uint64__bits64 key _value encoder =
  ((
      let _alias = _value in
      do {Protobuf.Encoder.key (key, Protobuf.Bits64) encoder;
       Protobuf.Encoder.bits64 (Uint64.to_int64 _alias) encoder})
  [@ocaml.warning "-A";]) ;

value float__bits32 key _value encoder =
  ((
      let _alias = _value in
      do {Protobuf.Encoder.key (key, Protobuf.Bits32) encoder;
       Protobuf.Encoder.bits32 (Int32.bits_of_float _alias) encoder})
  [@ocaml.warning "-A";]) ;

value float__bits64 key _value encoder =
  ((
      let _alias = _value in
      do {Protobuf.Encoder.key (key, Protobuf.Bits64) encoder;
       Protobuf.Encoder.bits64 (Int64.bits_of_float _alias) encoder})
  [@ocaml.warning "-A";]) ;

value string__bytes key _value encoder =
  ((
      let _alias = _value in
      do {Protobuf.Encoder.key (key, Protobuf.Bytes) encoder;
       Protobuf.Encoder.bytes (Bytes.of_string _alias) encoder})
  [@ocaml.warning "-A";]) ;

value bytes__bytes key _value encoder =
  ((
      let _alias = _value in
      do {Protobuf.Encoder.key (key, Protobuf.Bytes) encoder;
       Protobuf.Encoder.bytes _alias encoder})
  [@ocaml.warning "-A";]) ;

end ;

module Decode = struct
value int__varint = fun msg decoder ->
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (1, Protobuf.Varint) ->
            do {_alias.val :=
               (Some
                  (Protobuf.Decoder.int_of_int64 msg
                     (Protobuf.Decoder.varint decoder)));
             read () }
        | Some (1, kind) ->
            raise
              (let open Protobuf.Decoder in
                 Failure (Unexpected_payload msg kind))
        | Some (_, kind) -> do {Protobuf.Decoder.skip decoder kind; read ()}
        | None -> () ] in do {
      read ();
      (match _alias.val with [
        None ->
           raise
             (let open Protobuf.Decoder in
                Failure (Missing_field msg))
       | Some v -> v ]) })
  [@ocaml.warning "-A";]) ;

value bool__variant = fun msg decoder ->
((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (1, Protobuf.Varint) -> do {
            _alias.val :=
               (Some
                  (Protobuf.Decoder.bool_of_int64 msg
                     (Protobuf.Decoder.varint decoder)));
             read () }
        | Some (1, kind) ->
            raise
              (let open Protobuf.Decoder in
                 Failure (Unexpected_payload msg kind))
        | Some (_, kind) -> do { Protobuf.Decoder.skip decoder kind; read () }
        | None -> () ] in do {
      read ();
      (match _alias.val with [
         None ->
           raise
             (let open Protobuf.Decoder in
                Failure (Missing_field msg))
       | Some v -> v ])
      }
      )
  [@ocaml.warning "-A";]) ;

value int__zigzag msg decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
         Some (1, Protobuf.Varint) -> do
            {_alias.val :=
               (Some
                  (Protobuf.Decoder.int_of_int64 msg
                     (Protobuf.Decoder.zigzag decoder)));
             read ()}
        | Some (1, kind) ->
            raise
              (let open Protobuf.Decoder in
                 Failure (Unexpected_payload msg kind))
        | Some (_, kind) -> do {Protobuf.Decoder.skip decoder kind; read ()}
        | None -> () ] in do {
      read ();
      (match _alias.val with [
        None ->
           raise
             (let open Protobuf.Decoder in
                Failure (Missing_field msg))
       | Some v -> v ]) })
  [@ocaml.warning "-A";]) ;

value int__bits32 msg decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
         Some (1, Protobuf.Bits32) ->
            do {_alias.val :=
               (Some
                  (Protobuf.Decoder.int_of_int32 msg
                     (Protobuf.Decoder.bits32 decoder)));
             read ()}
        | Some (1, kind) ->
            raise
              (let open Protobuf.Decoder in
                 Failure (Unexpected_payload msg kind))
        | Some (_, kind) -> do {Protobuf.Decoder.skip decoder kind; read ()}
        | None -> () ] in
      do { read ();
      (match _alias.val with [
         None ->
           raise
             (let open Protobuf.Decoder in
                Failure (Missing_field msg))
       | Some v -> v ]) })
  [@ocaml.warning "-A";]) ;

value int__bits64 msg decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (1, Protobuf.Bits64) ->
            do {_alias.val :=
               (Some
                  (Protobuf.Decoder.int_of_int64 msg
                     (Protobuf.Decoder.bits64 decoder)));
             read ()}
        | Some (1, kind) ->
            raise
              (let open Protobuf.Decoder in
                 Failure (Unexpected_payload msg kind))
        | Some (_, kind) -> do {Protobuf.Decoder.skip decoder kind; read ()}
        | None -> () ] in
      do {read ();
      (match _alias.val with [
         None ->
           raise
             (let open Protobuf.Decoder in
                Failure (Missing_field msg))
       | Some v -> v ]) })
  [@ocaml.warning "-A";]) ;

value int32__varint msg decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
         Some (1, Protobuf.Varint) ->
            do {_alias.val :=
               (Some
                  (Protobuf.Decoder.int32_of_int64 msg
                     (Protobuf.Decoder.varint decoder)));
             read ()}
        | Some (1, kind) ->
            raise
              (let open Protobuf.Decoder in
                 Failure
                   (Unexpected_payload "Test_syntax.ml.ppx.il1" kind))
        | Some (_, kind) -> do {Protobuf.Decoder.skip decoder kind; read ()}
        | None -> () ] in
      do { read ();
      (match _alias.val with [
        None ->
           raise
             (let open Protobuf.Decoder in
                Failure (Missing_field "Test_syntax.ml.ppx.il1"))
       | Some v -> v ]) })
  [@ocaml.warning "-A";]) ;

value int32__zigzag msg decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (1, Protobuf.Varint) ->
            do {_alias.val :=
               (Some
                  (Protobuf.Decoder.int32_of_int64 msg
                     (Protobuf.Decoder.zigzag decoder)));
             read ()}
        | Some (1, kind) ->
            raise
              (let open Protobuf.Decoder in
                 Failure
                   (Unexpected_payload msg kind))
        | Some (_, kind) -> do {Protobuf.Decoder.skip decoder kind; read ()}
        | None -> () ] in
      do { read ();
      (match _alias.val with [
         None ->
           raise
             (let open Protobuf.Decoder in
                Failure (Missing_field msg))
       | Some v -> v ]) })
  [@ocaml.warning "-A";]);

value int32__bits32 msg decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (1, Protobuf.Bits32) ->
            do {_alias.val := (Some (Protobuf.Decoder.bits32 decoder)); read ()}
        | Some (1, kind) ->
            raise
              (let open Protobuf.Decoder in
                 Failure
                   (Unexpected_payload msg kind))
        | Some (_, kind) -> do {Protobuf.Decoder.skip decoder kind; read ()}
        | None -> () ] in
      do { read ();
      (match _alias.val with [
         None ->
           raise
             (let open Protobuf.Decoder in
                Failure (Missing_field msg))
       | Some v -> v ]) })
  [@ocaml.warning "-A";]) ;

value int32__bits64 msg decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (1, Protobuf.Bits64) ->
            do {_alias.val :=
               (Some
                  (Protobuf.Decoder.int32_of_int64 msg
                     (Protobuf.Decoder.bits64 decoder)));
             read ()}
        | Some (1, kind) ->
            raise
              (let open Protobuf.Decoder in
                 Failure
                   (Unexpected_payload msg kind))
        | Some (_, kind) -> do {Protobuf.Decoder.skip decoder kind; read ()}
        | None -> () ] in
      do { read ();
      (match _alias.val with [
         None ->
           raise
             (let open Protobuf.Decoder in
                Failure (Missing_field msg))
       | Some v -> v ]) })
  [@ocaml.warning "-A";]) ;

value int64__varint msg decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (1, Protobuf.Varint) ->
            do {_alias.val := (Some (Protobuf.Decoder.varint decoder)); read ()}
        | Some (1, kind) ->
            raise
              (let open Protobuf.Decoder in
                 Failure
                   (Unexpected_payload msg kind))
        | Some (_, kind) -> do {Protobuf.Decoder.skip decoder kind; read ()}
        | None -> () ] in
      do { read ();
      (match _alias.val with [
         None ->
           raise
             (let open Protobuf.Decoder in
                Failure (Missing_field msg))
       | Some v -> v ]) })
  [@ocaml.warning "-A";]) ;

value int64__zigzag msg decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (1, Protobuf.Varint) ->
            do {_alias.val := (Some (Protobuf.Decoder.zigzag decoder)); read ()}
        | Some (1, kind) ->
            raise
              (let open Protobuf.Decoder in
                 Failure
                   (Unexpected_payload msg kind))
        | Some (_, kind) -> do {Protobuf.Decoder.skip decoder kind; read ()}
        | None -> () ] in
      do { read ();
      (match _alias.val with [
         None ->
           raise
             (let open Protobuf.Decoder in
                Failure (Missing_field msg))
       | Some v -> v ]) })
  [@ocaml.warning "-A";]) ;

value int64__bits32 msg decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (1, Protobuf.Bits32) ->
            do {_alias.val :=
               (Some (Int64.of_int32 (Protobuf.Decoder.bits32 decoder)));
             read ()}
        | Some (1, kind) ->
            raise
              (let open Protobuf.Decoder in
                 Failure
                   (Unexpected_payload msg kind))
        | Some (_, kind) -> do {Protobuf.Decoder.skip decoder kind; read ()}
        | None -> () ] in
      do { read ();
      (match _alias.val with [
         None ->
           raise
             (let open Protobuf.Decoder in
                Failure (Missing_field msg))
       | Some v -> v ]) })
  [@ocaml.warning "-A";]) ;

value int64__bits64 msg decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (1, Protobuf.Bits64) ->
            do {_alias.val := (Some (Protobuf.Decoder.bits64 decoder)); read ()}
        | Some (1, kind) ->
            raise
              (let open Protobuf.Decoder in
                 Failure
                   (Unexpected_payload msg kind))
        | Some (_, kind) -> do {Protobuf.Decoder.skip decoder kind; read ()}
        | None -> () ] in
      do { read ();
      (match _alias.val with [
         None ->
           raise
             (let open Protobuf.Decoder in
                Failure (Missing_field msg))
       | Some v -> v ]) })
  [@ocaml.warning "-A";]) ;

value uint32__varint msg decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (1, Protobuf.Varint) ->
            do {_alias.val :=
               (Some
                  (Uint32.of_int32
                     (Protobuf.Decoder.int32_of_int64
                        msg
                        (Protobuf.Decoder.varint decoder))));
             read ()}
        | Some (1, kind) ->
            raise
              (let open Protobuf.Decoder in
                 Failure
                   (Unexpected_payload msg kind))
        | Some (_, kind) -> do {Protobuf.Decoder.skip decoder kind; read ()}
        | None -> () ] in
      do { read ();
      (match _alias.val with [
         None ->
           raise
             (let open Protobuf.Decoder in
                Failure (Missing_field msg))
       | Some v -> v ]) })
  [@ocaml.warning "-A";]) ;

value uint32__zigzag msg decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (1, Protobuf.Varint) ->
            do {_alias.val :=
               (Some
                  (Uint32.of_int32
                     (Protobuf.Decoder.int32_of_int64
                        msg
                        (Protobuf.Decoder.zigzag decoder))));
             read ()}
        | Some (1, kind) ->
            raise
              (let open Protobuf.Decoder in
                 Failure
                   (Unexpected_payload msg kind))
        | Some (_, kind) -> do {Protobuf.Decoder.skip decoder kind; read ()}
        | None -> () ] in
      do { read ();
      (match _alias.val with [
         None ->
           raise
             (let open Protobuf.Decoder in
                Failure (Missing_field msg))
       | Some v -> v ]) })
  [@ocaml.warning "-A";]) ;

value uint32__bits32 msg decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (1, Protobuf.Bits32) ->
            do {_alias.val :=
               (Some (Uint32.of_int32 (Protobuf.Decoder.bits32 decoder)));
             read ()}
        | Some (1, kind) ->
            raise
              (let open Protobuf.Decoder in
                 Failure
                   (Unexpected_payload msg kind))
        | Some (_, kind) -> do {Protobuf.Decoder.skip decoder kind; read ()}
        | None -> () ] in
      do { read ();
      (match _alias.val with [
         None ->
           raise
             (let open Protobuf.Decoder in
                Failure (Missing_field msg))
       | Some v -> v ]) })
  [@ocaml.warning "-A";]) ;

value uint32__bits64 msg decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (1, Protobuf.Bits64) ->
            do {_alias.val :=
               (Some
                  (Uint32.of_int32
                     (Protobuf.Decoder.int32_of_int64
                        msg
                        (Protobuf.Decoder.bits64 decoder))));
             read ()}
        | Some (1, kind) ->
            raise
              (let open Protobuf.Decoder in
                 Failure
                   (Unexpected_payload msg kind))
        | Some (_, kind) -> do {Protobuf.Decoder.skip decoder kind; read ()}
        | None -> () ] in
      do { read ();
      (match _alias.val with [
         None ->
           raise
             (let open Protobuf.Decoder in
                Failure (Missing_field msg))
       | Some v -> v ]) })
  [@ocaml.warning "-A";]) ;

value uint64__varint msg decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (1, Protobuf.Varint) ->
            do {_alias.val :=
               (Some (Uint64.of_int64 (Protobuf.Decoder.varint decoder)));
             read ()}
        | Some (1, kind) ->
            raise
              (let open Protobuf.Decoder in
                 Failure
                   (Unexpected_payload msg kind))
        | Some (_, kind) -> do {Protobuf.Decoder.skip decoder kind; read ()}
        | None -> () ] in
      do { read ();
      (match _alias.val with [
         None ->
           raise
             (let open Protobuf.Decoder in
                Failure (Missing_field msg))
       | Some v -> v ]) })
  [@ocaml.warning "-A";]) ;

value uint64__zigzag msg decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (1, Protobuf.Varint) ->
            do {_alias.val :=
               (Some (Uint64.of_int64 (Protobuf.Decoder.zigzag decoder)));
             read ()}
        | Some (1, kind) ->
            raise
              (let open Protobuf.Decoder in
                 Failure
                   (Unexpected_payload msg kind))
        | Some (_, kind) -> do {Protobuf.Decoder.skip decoder kind; read ()}
        | None -> () ] in
      do { read ();
      (match _alias.val with [
         None ->
           raise
             (let open Protobuf.Decoder in
                Failure (Missing_field msg))
       | Some v -> v ]) })
  [@ocaml.warning "-A";]) ;

value uint64__bits32 msg decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (1, Protobuf.Bits32) ->
            do {_alias.val :=
               (Some (Uint64.of_int32 (Protobuf.Decoder.bits32 decoder)));
             read ()}
        | Some (1, kind) ->
            raise
              (let open Protobuf.Decoder in
                 Failure
                   (Unexpected_payload msg kind))
        | Some (_, kind) -> do {Protobuf.Decoder.skip decoder kind; read ()}
        | None -> () ] in
      do { read ();
      (match _alias.val with [
         None ->
           raise
             (let open Protobuf.Decoder in
                Failure (Missing_field msg))
       | Some v -> v ]) })
  [@ocaml.warning "-A";]) ;

value uint64__bits64 msg decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (1, Protobuf.Bits64) ->
            do {_alias.val :=
               (Some (Uint64.of_int64 (Protobuf.Decoder.bits64 decoder)));
             read ()}
        | Some (1, kind) ->
            raise
              (let open Protobuf.Decoder in
                 Failure
                   (Unexpected_payload msg kind))
        | Some (_, kind) -> do {Protobuf.Decoder.skip decoder kind; read ()}
        | None -> () ] in
      do { read ();
      (match _alias.val with [
         None ->
           raise
             (let open Protobuf.Decoder in
                Failure (Missing_field msg))
       | Some v -> v ]) })
  [@ocaml.warning "-A";]) ;

value float__bits32 msg decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (1, Protobuf.Bits32) ->
            do {_alias.val :=
               (Some (Int32.float_of_bits (Protobuf.Decoder.bits32 decoder)));
             read ()}
        | Some (1, kind) ->
            raise
              (let open Protobuf.Decoder in
                 Failure (Unexpected_payload msg kind))
        | Some (_, kind) -> do {Protobuf.Decoder.skip decoder kind; read ()}
        | None -> () ] in
      do { read ();
      (match _alias.val with [
         None ->
           raise
             (let open Protobuf.Decoder in
                Failure (Missing_field msg))
       | Some v -> v ]) })
  [@ocaml.warning "-A";]);

value float__bits64 msg decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (1, Protobuf.Bits64) ->
            do {_alias.val :=
               (Some (Int64.float_of_bits (Protobuf.Decoder.bits64 decoder)));
             read ()}
        | Some (1, kind) ->
            raise
              (let open Protobuf.Decoder in
                 Failure (Unexpected_payload msg kind))
        | Some (_, kind) -> do {Protobuf.Decoder.skip decoder kind; read ()}
        | None -> () ] in
      do { read ();
      (match _alias.val with [
         None ->
           raise
             (let open Protobuf.Decoder in
                Failure (Missing_field msg))
       | Some v -> v ]) })
  [@ocaml.warning "-A";]) ;

value string__bytes msg decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (1, Protobuf.Bytes) ->
            do {_alias.val :=
               (Some (Bytes.to_string (Protobuf.Decoder.bytes decoder)));
             read ()}
        | Some (1, kind) ->
            raise
              (let open Protobuf.Decoder in
                 Failure (Unexpected_payload msg kind))
        | Some (_, kind) -> do {Protobuf.Decoder.skip decoder kind; read ()}
        | None -> () ] in
      do { read ();
      (match _alias.val with [
         None ->
           raise
             (let open Protobuf.Decoder in
                Failure (Missing_field msg))
       | Some v -> v ]) })
  [@ocaml.warning "-A";]) ;

value bytes__bytes msg decoder =
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (1, Protobuf.Bytes) ->
            do {_alias.val := (Some (Protobuf.Decoder.bytes decoder)); read ()}
        | Some (1, kind) ->
            raise
              (let open Protobuf.Decoder in
                 Failure (Unexpected_payload msg kind))
        | Some (_, kind) -> do {Protobuf.Decoder.skip decoder kind; read ()}
        | None -> () ] in
      do { read ();
      (match _alias.val with [
         None ->
           raise
             (let open Protobuf.Decoder in
                Failure (Missing_field msg))
       | Some v -> v ]) })
  [@ocaml.warning "-A";]) ;

end ;
