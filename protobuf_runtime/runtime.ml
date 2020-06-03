(* camlp5r *)
(* runtime.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

module Stdlib = Stdlib ;

module Protobuf = Protobuf ;

module Encode = struct
value int__varint = fun v encoder ->
((      let _alias = v in
      do { Protobuf.Encoder.key (1, Protobuf.Varint) encoder;
       Protobuf.Encoder.varint (Int64.of_int _alias) encoder}) (*  *)
  [@ocaml.warning "-A";]) ;

value bool__varint = fun v encoder ->
      let _alias = v in do {
        Protobuf.Encoder.key (1, Protobuf.Varint) encoder;
        Protobuf.Encoder.varint (if _alias then 1L else 0L) encoder
      }
  [@ocaml.warning "-A";] ;

end ;

module Decode = struct
value int__varint = fun decoder ->
  ((
      let _alias = ref None in
      let rec read () =
        match Protobuf.Decoder.key decoder with [
          Some (1, Protobuf.Varint) ->
            do {_alias.val :=
               (Some
                  (Protobuf.Decoder.int_of_int64 "Test_syntax.ml.ppx.i1"
                     (Protobuf.Decoder.varint decoder)));
             read () }
        | Some (1, kind) ->
            raise
              (let open Protobuf.Decoder in
                 Failure (Unexpected_payload "Test_syntax.ml.ppx.i1" kind))
        | Some (_, kind) -> do {Protobuf.Decoder.skip decoder kind; read ()}
        | None -> () ] in do {
      read ();
      (match _alias.val with [
        None ->
           raise
             (let open Protobuf.Decoder in
                Failure (Missing_field "Test_syntax.ml.ppx.i1"))
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
end ;
