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

let lengthspec_of_int len = function
    `INTELINT -> `INTELINT len
  | `NETINT -> `NETINT len
  | `SCHAR -> assert (len < 128) ; `SCHAR len
  | `SINTELSHORT -> assert (len < 32768) ; `SINTELSHORT len
  | `SNETSHORT -> assert (len < 32768) ; `SNETSHORT len
  | `UCHAR -> assert (len < 128) ; `UCHAR len
  | `UINTELSHORT -> assert (len < 32768) ; `UINTELSHORT len
  | `UNETSHORT -> assert (len < 32768) ; `UNETSHORT len

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

let int_of_hex_char = function
	'0'..'9' as c -> (Char.code c) - (Char.code '0')
  | 'a'..'f' as c -> (Char.code c) - (Char.code 'a') + 10
  | 'A'..'F' as c -> (Char.code c) - (Char.code 'A') + 10
  | _ -> assert false

let hex_char_of_int c =
  if 0 <= c && c <= 9 then
	Char.unsafe_chr(c + (Char.code '0'))
  else if 10 <= c && c <= 15 then
	Char.unsafe_chr(c - 10 + (Char.code 'a'))
  else
	assert false

let rec pack1 s ofs = function
    `UCHAR c -> (s.[ofs] <- Char.unsafe_chr c; ofs+1)

  | `SCHAR c -> (s.[ofs] <- Char.unsafe_chr c; ofs+1)

  | `SNETSHORT n -> (s.[ofs  ] <- Char.unsafe_chr (n asr 8);
					 s.[ofs+1] <- Char.unsafe_chr n;
					 ofs+2)

  | `UNETSHORT n -> (s.[ofs  ] <- Char.unsafe_chr (n asr 8);
					 s.[ofs+1] <- Char.unsafe_chr n;
					 ofs+2)

  | `SINTELSHORT n -> (s.[ofs+1] <- Char.unsafe_chr (n asr 8);
					   s.[ofs  ] <- Char.unsafe_chr n;
					   ofs+2)

  | `UINTELSHORT n -> (s.[ofs+1] <- Char.unsafe_chr (n asr 8);
					   s.[ofs  ] <- Char.unsafe_chr n;
					   ofs+2)

  | `NETINT n -> (s.[ofs  ] <- Char.unsafe_chr (n asr 24);
				  s.[ofs+1] <- Char.unsafe_chr (n asr 16);
				  s.[ofs+2] <- Char.unsafe_chr (n asr 8);
				  s.[ofs+3] <- Char.unsafe_chr n;
				  ofs+4)

  | `INTELINT n -> (s.[ofs+3] <- Char.unsafe_chr (n asr 24);
					s.[ofs+2] <- Char.unsafe_chr (n asr 16);
					s.[ofs+1] <- Char.unsafe_chr (n asr 8);
					s.[ofs  ] <- Char.unsafe_chr n;
				  ofs+4)

  | `NETINT32 n -> (s.[ofs  ] <- Char.unsafe_chr (Int32.to_int (Int32.logand (Int32.of_int 0xff) (Int32.shift_right n 24)));
					s.[ofs+1] <- Char.unsafe_chr (Int32.to_int (Int32.logand (Int32.of_int 0xff) (Int32.shift_right n 16)));
					s.[ofs+2] <- Char.unsafe_chr (Int32.to_int (Int32.logand (Int32.of_int 0xff) (Int32.shift_right n 8)));
					s.[ofs+3] <- Char.unsafe_chr (Int32.to_int n);
					ofs+4)

  | `INTELINT32 n -> (s.[ofs+3] <- Char.unsafe_chr (Int32.to_int (Int32.logand (Int32.of_int 0xff) (Int32.shift_right n 24)));
					  s.[ofs+2] <- Char.unsafe_chr (Int32.to_int (Int32.logand (Int32.of_int 0xff) (Int32.shift_right n 16)));
					  s.[ofs+1] <- Char.unsafe_chr (Int32.to_int (Int32.logand (Int32.of_int 0xff) (Int32.shift_right n 8)));
					  s.[ofs  ] <- Char.unsafe_chr (Int32.to_int n);
					ofs+4)

  | `NETINT64 n -> (s.[ofs  ] <- Char.unsafe_chr (Int64.to_int (Int64.logand (Int64.of_int 0xff) (Int64.shift_right n 56)));
		    s.[ofs+1] <- Char.unsafe_chr (Int64.to_int (Int64.logand (Int64.of_int 0xff) (Int64.shift_right n 48)));
		    s.[ofs+2] <- Char.unsafe_chr (Int64.to_int (Int64.logand (Int64.of_int 0xff) (Int64.shift_right n 40)));
		    s.[ofs+3] <- Char.unsafe_chr (Int64.to_int (Int64.logand (Int64.of_int 0xff) (Int64.shift_right n 32)));
		    s.[ofs+4] <- Char.unsafe_chr (Int64.to_int (Int64.logand (Int64.of_int 0xff) (Int64.shift_right n 24)));
		    s.[ofs+5] <- Char.unsafe_chr (Int64.to_int (Int64.logand (Int64.of_int 0xff) (Int64.shift_right n 16)));
		    s.[ofs+6] <- Char.unsafe_chr (Int64.to_int (Int64.logand (Int64.of_int 0xff) (Int64.shift_right n 8)));
		    s.[ofs+7] <- Char.unsafe_chr (Int64.to_int (Int64.logand (Int64.of_int 0xff) n));
		    ofs+8)

  | `INTELINT64 n -> (s.[ofs+7] <- Char.unsafe_chr (Int64.to_int (Int64.logand (Int64.of_int 0xff) (Int64.shift_right n 56)));
		      s.[ofs+6] <- Char.unsafe_chr (Int64.to_int (Int64.logand (Int64.of_int 0xff) (Int64.shift_right n 48)));
		      s.[ofs+5] <- Char.unsafe_chr (Int64.to_int (Int64.logand (Int64.of_int 0xff) (Int64.shift_right n 40)));
		      s.[ofs+4] <- Char.unsafe_chr (Int64.to_int (Int64.logand (Int64.of_int 0xff) (Int64.shift_right n 32)));
		      s.[ofs+3] <- Char.unsafe_chr (Int64.to_int (Int64.logand (Int64.of_int 0xff) (Int64.shift_right n 24)));
		      s.[ofs+2] <- Char.unsafe_chr (Int64.to_int (Int64.logand (Int64.of_int 0xff) (Int64.shift_right n 16)));
		      s.[ofs+1] <- Char.unsafe_chr (Int64.to_int (Int64.logand (Int64.of_int 0xff) (Int64.shift_right n 8)));
		      s.[ofs  ] <- Char.unsafe_chr (Int64.to_int n);
		      ofs+8)

  | `STRING str -> (String.blit str 0 s ofs (String.length str);
		    ofs+(String.length str))

  | `SUBSTRING (str,sofs,slen) -> (String.blit str sofs s ofs slen;
				   ofs+slen)

  | `SIZED_STRING(ispec,str) ->
      let len = String.length str in
      let ispec = lengthspec_of_int len ispec in
      let ofs = pack1 s ofs ispec in
	pack1 s ofs (`STRING str)

  | `SIZED_SUBSTRING(ispec,str,sofs,slen) ->
      let len = slen in
      let ispec = lengthspec_of_int len ispec in
      let ofs = pack1 s ofs ispec in
	pack1 s ofs (`SUBSTRING (str,sofs,slen))

  | `HEXBESTRING str ->
	  let len = String.length str in
	  let rec packrec n ofs =
		if n+1 < len then
		  let hi_c = str.[n] in
		  let lo_c = str.[n+1]
		  in
			s.[ofs] <- Char.unsafe_chr (((int_of_hex_char hi_c) lsl 4) + (int_of_hex_char lo_c));
			packrec (n+2) (ofs+1)
		else if n < len then
		  let hi_c = str.[n]
		  in
			s.[ofs] <- Char.unsafe_chr ((int_of_hex_char hi_c) lsl 4)
	  in
		(packrec 0 ofs;
		 ofs + (len+1)/2)

  | `HEXBESUBSTRING (str,src_ofs,src_len) ->
	  let len = src_len in
	    assert (src_ofs + len <= String.length str);
	  let rec packrec n ofs =
		if n+1 < src_ofs+len then
		  let hi_c = str.[n] in
		  let lo_c = str.[n+1]
		  in
			s.[ofs] <- Char.unsafe_chr (((int_of_hex_char hi_c) lsl 4) + (int_of_hex_char lo_c));
			packrec (n+2) (ofs+1)
		else if n < src_ofs+len then
		  let hi_c = str.[n]
		  in
			s.[ofs] <- Char.unsafe_chr ((int_of_hex_char hi_c) lsl 4)
	  in
		(packrec src_ofs ofs;
		 ofs + (len+1)/2)

  | `HEXLESTRING str ->
	  let len = String.length str in
	  let rec packrec n ofs =
		if n+1 < len then
		  let lo_c = str.[n] in
		  let hi_c = str.[n+1]
		  in
			s.[ofs] <- Char.unsafe_chr (((int_of_hex_char hi_c) lsl 4) + (int_of_hex_char lo_c));
			packrec (n+2) (ofs+1)
		else if n < len then
		  let lo_c = str.[n]
		  in
			s.[ofs] <- Char.unsafe_chr (int_of_hex_char lo_c)
	  in
		(packrec 0 ofs;
		 ofs + (len+1)/2)

let rec len1 = function
	`UCHAR _ -> 1
  | `SCHAR _ -> 1
  | (`SNETSHORT _|`SINTELSHORT _) -> 2
  | (`UNETSHORT _|`UINTELSHORT _) -> 2
  | (`NETINT _|`INTELINT _) -> 4
  | (`NETINT32 _|`INTELINT32 _) -> 4
  | (`NETINT64 _|`INTELINT64 _) -> 8
  | `STRING s -> String.length s
  | `SUBSTRING (_,_,len) -> len
  | `SIZED_STRING (ispec,s) -> len1 (lengthspec_of_int (String.length s) ispec) + len1 (`STRING s)
  | `SIZED_SUBSTRING (ispec,s,sofs,slen) -> len1 (lengthspec_of_int slen ispec) + len1 (`SUBSTRING(s,sofs,slen))
  | `HEXBESTRING s -> ((String.length s)+1)/2
  | `HEXBESUBSTRING (s,ofs,len) -> assert (ofs+len <= String.length s); (len+1)/2
  | `HEXLESTRING s -> ((String.length s)+1)/2

let pack_substring s ofs l =
  List.fold_left (fun n spec -> pack1 s n spec) ofs l

let pack l =
  let len = List.fold_left (fun n spec -> n + (len1 spec)) 0 l in
  let s = Bytes.make len '\000' in
  let _ = pack_substring s 0 l
  in
	s

let unpack_uchar s ofs = Char.code s.[ofs]

let unpack_schar s ofs = 
  let c = Char.code s.[ofs]
  in
	if (c > 127) then c - 256 else c

let unpack_unetshort s ofs =
  ((Char.code s.[ofs]) lsl 8) + (Char.code s.[ofs+1])

let unpack_uintelshort s ofs =
  ((Char.code s.[ofs+1]) lsl 8) + (Char.code s.[ofs])

let unpack_snetshort s ofs =
  let n = ((Char.code s.[ofs]) lsl 8) + ((Char.code s.[ofs+1]))
  in
	if n > 32767 then n - 65536 else n

let unpack_sintelshort s ofs =
  let n = ((Char.code s.[ofs+1]) lsl 8) + ((Char.code s.[ofs]))
  in
	if n > 32767 then n - 65536 else n

let unpack_netint s ofs =
  ((Char.code s.[ofs  ]) lsl 24) + ((Char.code s.[ofs+1]) lsl 16) +
  ((Char.code s.[ofs+2]) lsl 8) + (Char.code s.[ofs+3])

let unpack_intelint s ofs =
  ((Char.code s.[ofs+3]) lsl 24) + ((Char.code s.[ofs+2]) lsl 16) +
  ((Char.code s.[ofs+1]) lsl 8) + (Char.code s.[ofs])

let unpack_netint32 s ofs =
  let lo3 = ((Char.code s.[ofs+1]) lsl 16) + ((Char.code s.[ofs+2]) lsl 8) + ((Char.code s.[ofs+3])) in
  let n = (Char.code s.[ofs])
  in
	Int32.add (Int32.shift_left (Int32.of_int n) 24) (Int32.of_int lo3)

let unpack_intelint32 s ofs =
  let lo3 = ((Char.code s.[ofs+2]) lsl 16) + ((Char.code s.[ofs+1]) lsl 8) + ((Char.code s.[ofs])) in
  let n = (Char.code s.[ofs+3])
  in
	Int32.add (Int32.shift_left (Int32.of_int n) 24) (Int32.of_int lo3)

let unpack_net_number lsl_f land_f lor_f inj_f val_zero val_ff nbytes =
  let unpack s ofs =
    let bound = ofs + nbytes in
    let rec unprec acc ofs =
      if ofs = bound then
	acc
      else
	unprec (lor_f (lsl_f acc) (land_f val_ff (inj_f (Char.code (String.get s ofs))))) (ofs+1)
    in
      unprec val_zero ofs
  in unpack

let unpack_intel_number lsl_f land_f lor_f inj_f val_zero val_ff nbytes =
  let unpack s low =
    let rec unprec acc ofs =
      if ofs < low then
	acc
      else
	unprec (lor_f (lsl_f acc) (land_f val_ff (inj_f (Char.code (String.get s ofs))))) (ofs-1)
    in
      unprec val_zero (low + nbytes - 1)
  in unpack

let unpack_netint' = unpack_net_number (fun n -> (n lsl 8)) (land) (lor) (fun n -> n) 0 0xff 4
let unpack_intelint' = unpack_intel_number (fun n -> (n lsl 8)) (land) (lor) (fun n -> n) 0 0xff 4

let unpack_netint64 = unpack_net_number (fun n -> Int64.shift_left n 8) Int64.logand Int64.logor Int64.of_int Int64.zero (Int64.of_int 0xff) 8
let unpack_intelint64 = unpack_intel_number (fun n -> Int64.shift_left n 8) Int64.logand Int64.logor Int64.of_int Int64.zero (Int64.of_int 0xff) 8

let unpack_n siz unpacker n s ofs =
  let rec unprec ofs = function
	  0 -> []
	| n ->
		(unpacker s ofs)::(unprec (ofs+siz) (n-1))
  in
	unprec ofs n

let unpack_hexbestring n s ofs =
  let dlen = n * 2 in
  let dst = Bytes.make dlen '\000' in

  let sbound = ofs + n in
  let rec unrec spos dpos =
	if spos < sbound then
	  let c = Char.code s.[spos] in

		dst.[dpos  ] <- hex_char_of_int ((c asr 4) land 0x0f);
		dst.[dpos+1] <- hex_char_of_int (c land 0x0f);
		unrec (spos+1) (dpos+2)

	else if spos < sbound then
	  let c = Char.code s.[spos] in

		dst.[dpos  ] <- hex_char_of_int ((c asr 4) land 0xf0)
  in
	unrec ofs 0;
	dst

let unpack_hexlestring n s ofs =
  let dlen = n * 2 in
  let dst = Bytes.make dlen '\000' in

  let sbound = ofs + n in
  let rec unrec spos dpos =
	if spos < sbound then
	  let c = Char.code s.[spos] in

		dst.[dpos+1] <- hex_char_of_int ((c asr 4) land 0x0f);
		dst.[dpos  ] <- hex_char_of_int (c land 0x0f);
		unrec (spos+1) (dpos+2)

	else if spos < sbound then
	  let c = Char.code s.[spos] in

		dst.[dpos  ] <- hex_char_of_int (c land 0x0f)
  in
	unrec ofs 0;
	dst

let rec unpack1 s ofs = function
	`UCHAR n -> (`UCHAR (unpack_n 1 unpack_uchar n s ofs),ofs+n)

  | `SCHAR n -> (`SCHAR (unpack_n 2 unpack_schar n s ofs), ofs+n)

  | `UNETSHORT n -> (`UNETSHORT (unpack_n 2 unpack_unetshort n s ofs), ofs+(2*n))

  | `UINTELSHORT n -> (`UINTELSHORT (unpack_n 2 unpack_uintelshort n s ofs), ofs+(2*n))

  | `SNETSHORT n -> (`SNETSHORT (unpack_n 2 unpack_snetshort n s ofs), ofs+(2*n))

  | `SINTELSHORT n -> (`SINTELSHORT (unpack_n 2 unpack_sintelshort n s ofs), ofs+(2*n))

  | `NETINT n -> (`NETINT (unpack_n 4 unpack_netint n s ofs), ofs+(4*n))

  | `INTELINT n -> (`INTELINT (unpack_n 4 unpack_intelint n s ofs), ofs+(4*n))

  | `NETINT32 n -> (`NETINT32 (unpack_n 4 unpack_netint32 n s ofs), ofs+(4*n))

  | `INTELINT32 n -> (`INTELINT32 (unpack_n 4 unpack_intelint32 n s ofs), ofs+(4*n))

  | `NETINT64 n -> (`NETINT64 (unpack_n 8 unpack_netint64 n s ofs), ofs+(8*n))

  | `INTELINT64 n -> (`INTELINT64 (unpack_n 8 unpack_intelint64 n s ofs), ofs+(8*n))

  | `STRING n -> (`STRING (String.sub s ofs n), ofs+n)

  | `SIZED_STRING ispec ->
      let ispec = lengthspec_of_int 1 ispec in
      let (len,ofs) = unpack1 s ofs ispec in
      let len = match len with
	  `INTELINT [n] -> n
	| `NETINT [n] -> n
	| `SCHAR [n] -> n
	| `SINTELSHORT [n] -> n
	| `SNETSHORT [n] -> n
	| `UCHAR [n] -> n
	| `UINTELSHORT [n] -> n
	| `UNETSHORT [n] -> n
	| _ -> assert false in
	unpack1 s ofs (`STRING len)

  | `HEXBESTRING n -> (`HEXBESTRING (Bytes.to_string (unpack_hexbestring n s ofs)), ofs+((n+1)/2))

  | `HEXLESTRING n -> (`HEXLESTRING (Bytes.to_string (unpack_hexlestring n s ofs)), ofs+((n+1)/2))

let unpack_substring0 s ofs l =
  let (ofs,acc) =
	List.fold_left
	  (fun (n,acc) spec ->
		 let (v,n') = unpack1 s n spec
		 in (n',v::acc))
	  (ofs,[])
	  l in
  (ofs, List.rev acc)

let unpack_substring s ofs l =
  snd (unpack_substring0 s ofs l)

(*	
                   a   A string with arbitrary binary data, will be null padded.
                   A   An ascii string, will be space padded.
                   Z   A null terminated (asciz) string, will be null padded.

                   b   A bit string (ascending bit order, like vec()).
                   B   A bit string (descending bit order).
                   h   A hex string (low nybble first).
                   H   A hex string (high nybble first).

                   c   A signed char value.
                   C   An unsigned char value.

                   s   A signed short value.
                   S   An unsigned short value.
                         (This 'short' is _exactly_ 16 bits, which may differ from
                          what a local C compiler calls 'short'.)

                   i   A signed integer value.
                   I   An unsigned integer value.
                         (This 'integer' is _at least_ 32 bits wide.  Its exact
                          size depends on what a local C compiler calls 'int',
                          and may even be larger than the 'long' described in
                          the next item.)

                   l   A signed long value.
                   L   An unsigned long value.
                         (This 'long' is _exactly_ 32 bits, which may differ from
                          what a local C compiler calls 'long'.)

                   n   A short in "network" (big-endian) order.
                   N   A long in "network" (big-endian) order.
                   v   A short in "VAX" (little-endian) order.
                   V   A long in "VAX" (little-endian) order.
                         (These 'shorts' and 'longs' are _exactly_ 16 bits and
                          _exactly_ 32 bits, respectively.)

28/Mar/1999            perl 5.005, patch 03                    55

                   q   A signed quad (64-bit) value.
                   Q   An unsigned quad value.
                         (Available only if your system supports 64-bit integer values
                          _and_ if Perl has been compiled to support those.
                          Causes a fatal error otherwise.)

                   f   A single-precision float in the native format.
                   d   A double-precision float in the native format.

                   p   A pointer to a null-terminated string.
                   P   A pointer to a structure (fixed-length string).

                   u   A uuencoded string.

                   w   A BER compressed integer.  Its bytes represent an unsigned
                       integer in base 128, most significant digit first, with as
                       few digits as possible.  Bit eight (the high bit) is set
                       on each byte except the last.

                   x   A null byte.
                   X   Back up a byte.
                   @   Null fill to absolute position.
*)
