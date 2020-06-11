
let hex_of_bytes b =
  let s = Bytes.to_string b in
  match Pack.unpack_substring s 0 [`HEXBESTRING (String.length s)] with
    [`HEXBESTRING hex] -> hex
  | _ -> assert false

let bytes_of_hex s =
  Pack.pack [`HEXBESTRING s]

