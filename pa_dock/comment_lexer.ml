(* camlp5r *)
(* plexer.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

#load "pa_lexer.cmo";

type context =
  { simplest_raw_strings : bool ;
    line_nb : ref int ;
    bol_pos : ref int ;
    bp_offset : int ;
    line_cnt : int -> char -> unit;
    make_lined_loc : (int * int) -> string -> Ploc.t }
;

(** [line_nb], [bol_pos], [bp_offset] are all absolute.

    Since bp/ep from lexer is always relative to the start of the
   string, in order to convert it to bol_pos we must add bp_offset to
   it. *)
   
value make_ctx loc =
    let line_nb = ref (Ploc.line_nb loc) in
    let bol_pos = ref (Ploc.bol_pos loc) in
    let bp_offset = Ploc.first_pos loc in
    {simplest_raw_strings = True ;
     line_nb = line_nb ; bol_pos = bol_pos ;
     bp_offset = bp_offset ;
 line_cnt bp1 c =
       match c with
       [ '\n' | '\r' -> do {
           if c = '\n' then incr line_nb else ();
           bol_pos.val := (bp1 + 1) + bp_offset;
         }
       | c -> () ];
     make_lined_loc (bp, ep) comm =
       Ploc.make_loc (Ploc.file_name loc) line_nb.val bol_pos.val (bp + bp_offset, ep + bp_offset) comm}
;

value stream_peek_nth n strm =
  loop n (Stream.npeek n strm) where rec loop n =
    fun
    [ [] -> None
    | [x] -> if n == 1 then Some x else None
    | [_ :: l] -> loop (n - 1) l ]
;

value err ctx loc msg =
  Ploc.raise (ctx.make_lined_loc loc "") (Plexing.Error msg)
;

value rec linedir n s =
  match stream_peek_nth n s with
  [ Some (' ' | '\t') -> linedir (n + 1) s
  | Some ('0'..'9') -> linedir_digits (n + 1) s
  | _ -> False ]
and linedir_digits n s =
  match stream_peek_nth n s with
  [ Some ('0'..'9') -> linedir_digits (n + 1) s
  | _ -> linedir_quote n s ]
and linedir_quote n s =
  match stream_peek_nth n s with
  [ Some (' ' | '\t') -> linedir_quote (n + 1) s
  | Some '"' -> True
  | _ -> False ]
;

value rec any_to_nl =
  lexer
  [ "\r" | "\n"
  | _ any_to_nl!
  | ]
;

value any ctx buf =
  parser bp [: `c :] -> do { ctx.line_cnt bp c; $add c }
;

value rec skiplws = lexer [
  ' '/ skiplws!
| '\t'/ skiplws!
|
]
;

value rec string ctx bp =
  lexer
  [ "\""/
  | "\\"/ ?= [ "\n" ] "\n"/ skiplws! (string ctx bp)!
  | "\\"/ ?= [ "\n" | " " ] (any ctx) (string ctx bp)!
  | "\\" (any ctx) (string ctx bp)!
  | (any ctx) (string ctx bp)!
  | -> err ctx (bp, $pos) "string not terminated" ]
;

value rec rawstring1 delimtok (ofs, delim) ctx buf =
  parser bp [: `c ; strm :] -> do {
    ctx.line_cnt bp c;
    let buf = $add c in
    if String.get delim ofs <> c then
       if String.get delim 0 = c then
         rawstring1 delimtok (1, delim) ctx buf strm
       else
         rawstring1 delimtok (0, delim) ctx buf strm
    else if ofs+1 < String.length delim then
      rawstring1 delimtok (ofs+1, delim) ctx buf strm
    else
      let s = $buf in
      let slen = String.length s in do {
      (delimtok, String.sub s 0 (slen - (String.length delim)))
      }
  }
;

value rec rawstring0 ctx bp buf =
  parser bp [
    [: `'|' ; strm :] -> do {
      rawstring1 $buf (0, "|" ^ $buf ^ "}") ctx $empty strm
    }
  | [: `('a'..'z' | '_' as c) ; strm :] -> do {
      rawstring0 ctx bp ($add c) strm
    }
  ]
;

value add_string buf s =
  let slen = String.length s in
  let rec addrec buf i =
    if i = slen then buf
    else addrec ($add (String.get s i)) (i+1)
  in addrec buf 0
;

(*
 * This predicate checks that the stream contains a valid raw-string starter.  
 * The definition of "valid raw string starter" depends on the value of 
 * the variable [simplest_raw_strings]: if it is [False], then a valid
 * raw-string starter is "[:alpha:]+|"; if it is [True], a valid raw-string
 * starter is "[:alpha:]*|".  [simplest_raw_strings] is set to True in
 * original syntax.

 * This predicate gets called when the main lexer has already seen a "{".
*)
value raw_string_starter_p ctx strm =
  let rec predrec n =
    match stream_peek_nth n strm with
      [ None -> False
      | Some ('a'..'z' | '_') ->
         predrec (n+1)
      | Some '|' when ctx.simplest_raw_strings || n > 1 -> True
      | Some _ -> False ]
  in predrec 1
;

value comment_rawstring ctx bp (buf : Plexing.Lexbuf.t) strm =
  if not (raw_string_starter_p ctx strm) then
    buf
  else
  let (delim, s) = rawstring0 ctx bp $empty strm in
  let rs = Printf.sprintf "{%s|%s|%s}" delim s delim in
  add_string buf rs
;

value comment ctx bp =
  comment where rec comment =
    lexer
    [ "*)"
    | "*" comment!
    | "{" (comment_rawstring ctx bp)! comment!
    | "(*" comment! comment!
    | "(" comment!
    | "\"" (string ctx bp)! [ -> $add "\"" ] comment!
    | "'*)"
    | "'*" comment!
    | "'" (any ctx) comment!
    | (any ctx) comment!
    | -> err ctx (bp, $pos) "comment not terminated" ]
;

value ws1 ctx buf =
  parser bp
  [ [: `('\n' | '\r' as c) :] -> do {
      ctx.line_cnt bp c ;
      ($add c)
    }
  | [: `(' ' | '\t' | '\026' | '\012' as c) :] -> do {
      ($add c)
    }
  | [: `'#' when bp = ctx.bol_pos.val; s :] ->
      if linedir 1 s then do {
        let buf = any_to_nl ($add '#') s in
        ctx.line_cnt ((Stream.count s)-1) '\n' ;
        buf
      }
      else err ctx (bp, $pos) "found # while parsing interstitial space"
  ]
;

value rec ws_star ctx buf =
  parser
  [ [: buf = ws1 ctx buf ; s :] ->
    ws_star ctx buf s
  | [: :] -> $buf ]
;

value rec ws_token ctx =
  parser bp
  [ [: buf = ws1 ctx $empty ; s :] -> ws_star ctx buf s
  ]
;

value rec next_token ctx =
  let buf = $empty in
  parser bp
  [ [: ws = ws_token ctx :] ep ->
    (ws, bp + ctx.bp_offset)

  | [: `'(';
       a =
         parser
         [ [: `'*'; buf = comment ctx bp ($add "(*") ! :] ep -> do {
             ($buf, bp + ctx.bp_offset)
           }
         | [: :] -> err ctx (bp, $pos) "found a ( while parsing interstitial space"
         ] ! :] -> a
  ]
;

value rec tokenize_all ctx =
  let rec trec acc = parser [
    [: t = next_token ctx ; s :] -> trec [t::acc] s
  | [: :] -> List.rev acc
  ] in trec []
;

value string_count s c = do {
  let cnt = ref 0 in
  for i = 0 to (String.length s) - 1 do {
    if c = s.[i] then incr cnt else ()
  } ;
  cnt.val
}
;

value adjust_loc s loc = do {
  let slines = string_count s '\n' in
  let slen = String.length s in
  let file_name = Ploc.file_name loc in
  let line_nb = Ploc.line_nb loc - slines in
  assert (line_nb > 0) ;
  let bp = Ploc.first_pos loc - slen in
  assert (bp >= 0) ;
  let ep = Ploc.last_pos loc - slen in
  assert (ep >= 0) ;
  let bol_pos = bp in
  assert (bol_pos >= 0) ;
  Ploc.make_loc file_name line_nb bol_pos (bp, ep) ""
}
;

value tokenize_comment (s,loc) =
  let is = Stream.of_string s in
  let ctx = make_ctx loc in
  tokenize_all ctx is
;
