(* Copyright 2016 Chetan Murthy *)


type length_pack_spec_t =
    [ `INTELINT
    | `NETINT
    | `SCHAR
    | `SINTELSHORT
    | `SNETSHORT
    | `UCHAR
    | `UINTELSHORT
    | `UNETSHORT]

type pack_spec_t =
    [ `HEXBESTRING of string
    | `HEXBESUBSTRING of string * int * int
    | `HEXLESTRING of string
    | `INTELINT of int
    | `INTELINT32 of int32
    | `INTELINT64 of int64
    | `NETINT of int
    | `NETINT32 of int32
    | `NETINT64 of int64
    | `SCHAR of int
    | `SINTELSHORT of int
    | `SNETSHORT of int
    | `STRING of string
    | `SUBSTRING of string * int * int
    | `SIZED_STRING of length_pack_spec_t * string
    | `SIZED_SUBSTRING of length_pack_spec_t * string * int * int
    | `UCHAR of int
    | `UINTELSHORT of int
    | `UNETSHORT of int]

type unpack_spec_t =
    [ `HEXBESTRING of int
    | `HEXLESTRING of int
    | `INTELINT of int
    | `INTELINT32 of int
    | `INTELINT64 of int
    | `NETINT of int
    | `NETINT32 of int
    | `NETINT64 of int
    | `SCHAR of int
    | `SINTELSHORT of int
    | `SNETSHORT of int
    | `STRING of int
    | `SIZED_STRING of length_pack_spec_t
    | `UCHAR of int
    | `UINTELSHORT of int
    | `UNETSHORT of int]

type unpack_result_t =
    [ `HEXBESTRING of string
    | `HEXLESTRING of string
    | `INTELINT of int list
    | `INTELINT32 of int32 list
    | `INTELINT64 of int64 list
    | `NETINT of int list
    | `NETINT32 of int32 list
    | `NETINT64 of int64 list
    | `SCHAR of int list
    | `SINTELSHORT of int list
    | `SNETSHORT of int list
    | `STRING of string
    | `UCHAR of int list
    | `UINTELSHORT of int list
    | `UNETSHORT of int list]

val pack_substring : bytes -> int -> pack_spec_t list -> int

val pack : pack_spec_t list -> bytes

val unpack_substring0 : string -> int -> unpack_spec_t list -> int * unpack_result_t list

val unpack_substring : string -> int -> unpack_spec_t list -> unpack_result_t list
