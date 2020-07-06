(****************************************************************************)
(*                 The Calculus of Inductive Constructions                  *)
(*                                                                          *)
(*                                Projet Coq                                *)
(*                                                                          *)
(*                     INRIA        LRI-CNRS        ENS-CNRS                *)
(*              Rocquencourt         Orsay          Lyon                    *)
(*                                                                          *)
(*                                 Coq V6.2                                 *)
(*                               May 1st 1998                               *)
(*                                                                          *)
(****************************************************************************)
(*                                  std.ml                                  *)
(****************************************************************************)

(* glob.ml *)

let pair a b = (a,b)

let fst3 (a,b,c)  = a
let snd3 (a,b,c)  = b
let third3 (a,b,c)  = c

let fst4 (a,b,c,d)  = a
let snd4 (a,b,c,d)  = b
let third4 (a,b,c,d)  = c
let fourth4 (a,b,c,d)  = d

let car = List.hd
let cdr = List.tl
let cadr x = List.hd (List.tl x)
let cddr x = List.tl (List.tl x)
let caddr x = car (cddr x)
let cdddr x = List.tl (List.tl (List.tl x))

let safe f e dflt =
  try f e
  with Failure _ -> dflt

let safe_cdr l = safe cdr l []
let safe_cddr l = safe cddr l []
let safe_cdddr l = safe cdddr l []


let except e =
 let rec except_e = function
     [] -> []
   | elem::l -> if e = elem then except_e l else elem::except_e l
 in except_e

(* prel.ml *)

let try_find f = 
 let rec try_find_f = function
     [] -> failwith "try_find"
   | h::t -> try f h with Failure _ -> try_find_f t
 in try_find_f

let filter p =
  let rec filter_aux = function
      [] -> []
    | x::l -> if p x then x::filter_aux l else filter_aux l
  in filter_aux

let rec distinct = function
    h::t -> (not (List.mem h t)) && distinct t
  | _ -> true

let nth1 l n =
  assert (n >= 0);
  List.nth l (n-1)

let rec last = function
    [] -> failwith "last"
  | x::[] -> x
  | x::l -> last l


let firstn n l =
  let rec aux acc = function
      (0, l) -> List.rev acc
    | (n, (h::t)) -> aux (h::acc) (pred n, t)
    | _ -> failwith "firstn"
  in aux [] (n,l)

let sep_firstn n l =
  let rec srec acc n l =
    if n = 0 then (List.rev acc, l)
    else
      match l with
      | [] -> failwith "sep_firstn"
      | h::l -> srec (h::acc) (n-1) l
  in srec [] n l

let rec sep_last = function
    [] -> failwith "sep_last"
  | hd::[] -> (hd,[])
  | hd::tl ->
      let (l,tl) = sep_last tl in (l,hd::tl)

let interval n m = 
  let rec interval_n (l,m) =
    if n > m then l else interval_n (m::l,pred m)
  in interval_n ([],m)

let interval_int32 n m =
  List.map Int32.of_int (interval (Int32.to_int n) (Int32.to_int m))

let range = interval 1
let range_int32 = interval_int32 1l

(* hash.ml *)

let map_succeed f = 
 let rec map_f =
 function [] -> []
 |  h::t -> try (let x = f h in x :: map_f t) with Failure _ -> map_f t
 in map_f 

let map3 f l1 l2 l3 =
  let rec map_f = function
    | [], [], [] ->[]
    | h1::t1, h2::t2, h3::t3 -> (f h1 h2 h3)::(map_f (t1, t2, t3))
    | _ -> invalid_arg "Std.map3"
  in map_f (l1, l2, l3)

let push l x = (l := x :: !l)

let pop l =
    match !l with
    h::tl -> l := tl
  | [] -> invalid_arg "pop"


let top l = List.hd !l

type ('a,'b) union = Inl of 'a | Inr of 'b

let isl = function
	Inl _ -> true
  | _ -> false

let isr = function
  | Inr _ -> true
  | _ -> false

let outl = function
    (Inl e) -> e
  | _ -> failwith "std__outl"

let outr = function
    (Inr e) -> e
  | _ -> failwith "std__outr"

(**********************************************)
(**** Functions on lists representing sets ****)

let rec uniquize = function
    [] -> []
  | (h::t) -> if List.mem h t then uniquize t else h::(uniquize t)
let make_set = uniquize

let add_set a fs = if List.mem a fs then fs else (a::fs)

let rec rmv_set a ls =
  match ls with
      (h::t) -> if h = a then t else h::(rmv_set a t)
    | _ -> failwith "listset__rmv"

let intersect l1 l2 = filter (fun x -> List.mem x l2) l1

let union l1 l2 =
  let rec union_rec = function
      [] -> l2
    | a::l -> if List.mem a l2 then union_rec l else a :: union_rec l
  in union_rec l1

let unionq l1 l2 = 
 let rec urec = function
     [] -> l2
   | a::l -> if List.memq a l2 then urec l else a::urec l
 in urec l1

let union2 l1 l2 =
  let htab = Hashtbl.create 151 in
  let rec add_elt l = function
      [] -> l
    | x::ll -> try Hashtbl.find htab x; add_elt l ll
      with Not_found -> Hashtbl.add htab x ();
        add_elt (x::l) ll
  in 
  let l' = add_elt [] l1
  in add_elt l' l2

let diff_set l1 l2 =
  if l2 = [] then l1 else filter (fun x -> not (List.mem x l2)) l1
let subtract = diff_set

let subtractq l1 l2 = filter (fun x -> not (List.memq x l2)) l1

let symdiff l1 l2 = diff_set (union l1 l2) (intersect l1 l2)

let subset l1 l2 =
  let t2 = Hashtbl.create 151 in
    List.iter (fun x-> Hashtbl.add t2 x ()) l2;
    let rec look = function
    [] -> true
      | x::ll -> try Hashtbl.find t2 x; look ll
        with Not_found -> false
    in look l1

let same_members s1 s2 = subset s1 s2 && subset s2 s1

let map_option f = function
	None -> None
  | Some x -> Some(f x)

let isNone = function None -> true | Some _ -> false
let isSome = function Some _ -> true | None -> false
let inSome n = Some n
let outSome (Some n) = n


let plist elem = 
  let rec plist_rec accum = parser
     [< e = elem; strm >] -> plist_rec (e::accum) strm
   | [< >]                         -> (List.rev accum)
  in plist_rec []

let plist_until terminator elem = 
  let rec plist_rec = parser
	  [< rv=terminator >] -> rv
    | [< e = elem; l = plist_rec >] -> e::l
  in plist_rec


let parse_fully f strm =
  let v = f strm
  in (parser
	  [< '_ >] -> raise (Stream.Error "parse_fully")
	| [< >] -> v) strm

let string_suffix s n = String.sub s n ((String.length s) - n)
let fold_option f base = function
    None -> base
  | Some v -> f v

let starts_with ?(improper=false) ~pat s =
  let slen = String.length s in
  let patlen = String.length pat in
    if slen < patlen || (slen = patlen && not improper) then
      false
    else
      (String.sub s 0 patlen) = pat

let ends_with ~pat s =
  let slen = String.length s in
  let patlen = String.length pat in
    if slen < patlen then
      false
    else
      (String.sub s (slen-patlen) patlen) = pat


let apply_to_in_channel f fna =
  let open Base.Exn in
  let ic = open_in fna in
    protect ~f:(fun () -> f ic)
      ~finally:(fun () -> close_in ic)

let apply_to_out_channel f fna =
  let oc = try open_out fna
    with Failure _ -> 
      failwith "apply_to_out_channel: Cannot open file for write" in
  try
    let rv = f oc
    in close_out oc; rv
  with exc -> (close_out oc; raise exc)

let sizebin_of_string s =
  let slen = String.length s in
    if slen = 0 then int_of_string s else
  let lastc = String.get s (slen-1) in
    match lastc with
    | 'k'|'K' ->
       1024 * (int_of_string (String.sub s 0 (slen-1)))
    | 'm'|'M' ->
       1024 * 1024 * (int_of_string (String.sub s 0 (slen-1)))
    | 'g'|'G' ->
       1024 * 1024 * 1024 * (int_of_string (String.sub s 0 (slen-1)))
    | _ -> int_of_string s
       
let sizebin64_of_string s =
  let slen = String.length s in
    if slen = 0 then Int64.of_string s else
  let lastc = String.get s (slen-1) in
    match lastc with
    | 'k'|'K' ->
       Int64.(mul 1024L (of_string (String.sub s 0 (slen-1))))
    | 'm'|'M' ->
       Int64.(mul 1024L (mul 1024L (of_string (String.sub s 0 (slen-1)))))
    | 'g'|'G' ->
       Int64.(mul 1024L (mul 1024L (mul 1024L (of_string (String.sub s 0 (slen-1))))))
    | _ -> Int64.of_string s

let string_of_sizebin64 n =
  let onek = Int64.(shift_left 1L 10) in
  let onem = Int64.(shift_left 1L 20) in
  let oneg = Int64.(shift_left 1L 30) in
  if Int64.(rem n oneg) = 0L then
    Printf.sprintf "%LdG" Int64.(div n oneg)
  else if Int64.(rem n onem) = 0L then
    Printf.sprintf "%LdM" Int64.(div n onem)
  else if Int64.(rem n onek) = 0L then
    Printf.sprintf "%LdK" Int64.(div n onek)
      else Printf.sprintf "%Ld" n
	
let string_of_sizebin n =
  string_of_sizebin64 (Int64.of_int n)

let do_option f = function
    Some x -> f x
  | None -> ()

let implode_chars cl =
  let len = List.length cl in
  let dest = Bytes.create len in
  let _ = 
    List.fold_left
      (fun start src -> Bytes.set dest start src; start + 1)
      0 cl
  in
    Bytes.to_string dest

let list_of_stream strm =
let rec listrec acc = parser
  [< 't ; strm >] -> listrec (t::acc) strm
| [< >] -> List.rev acc
in listrec [] strm

(* [nway_partition P l] partition the list [l] into a list of lists [L],
   such that each value [x] in [L] has the property that for every two
   elements [a],[b] in [x], [P a b] returns [true].

   ASSUMPTION: P is a transitive binary relation -- that is, if [P x y]
   and [P y z], then [P x z] will hold.  This is NOT checked.
*)

let nway_partition pred l =
  let rec partrec parts = function
  [] -> parts
    | h::t ->
       let rec insrec rejl = function
       [] -> [h]::rejl
	 | x::l ->
	    if (pred h (List.hd x)) then
	      (h::x)::(List.rev_append l rejl)
	    else
	      insrec (x::rejl) l
       in partrec (insrec [] parts) t
  in List.map List.rev (partrec [] l)

let read_ic_fully ?(msg="") ?(channel=stdin) () =
  let fd = Unix.descr_of_in_channel channel in
  if Unix.isatty fd && msg <> "" then begin
    Printf.printf "%s\n" msg ; flush stdout ;
  end ;
  let b = Buffer.create 23 in
  let rec rrec () =
    match try Some(input_char channel)
      with End_of_file -> None with
	None -> Buffer.contents b
      | Some c -> Buffer.add_char b c ; rrec ()
  in
  rrec()

(* [format_columns ~width l]

   formats the argument list [l] into columns like UNIX ls(1).

   The algorithm is weak, weak, weak.  We select a # of columns, and
   compute the column-width of each column, using that # of columns.
   If that's too wide for the specified terminal-width, this # of
   columns fails.

   We start by trying 2 columns and if that fails we go with one
   column; if it succeeds, we increase unti we fail.

*)

module ColumnFormat = struct

let format0 ncols l =
  let n = List.length l in
  let nrows = (n / ncols) + (if n mod ncols = 0 then 0 else 1) in
  let formatted = Array.make_matrix ncols nrows "" in
  List.iteri (fun i s ->
    let col = i / nrows in
    let row = i mod nrows in
    formatted.(col).(row) <- s
  ) l ;
  formatted

let formatted_width ~ncols l =
  let fmt = format0 ncols l in
  let colwidths = Array.make ncols 0 in
  for i = 0 to ncols - 1 do
    colwidths.(i) <- Array.fold_left (fun acc s -> max acc (String.length s)) 0 fmt.(i)
  done ;
  for i = 0 to ncols - 2 do
    colwidths.(i) <- 2 + colwidths.(i)
  done ;
  let width = Array.fold_left (+) 0 colwidths in
  width, colwidths, fmt
    
let format ~width l =
  let rec search n =
    let (cur_width, colwidths, fmt) = formatted_width n l in
    if cur_width > width then None
    else match search (n+1) with
    | None -> Some (cur_width, colwidths, fmt)
    | Some x ->  Some x
  in
  match search 2 with
  | None -> [|Array.of_list l|]
  | Some (cur_width, colwidths, fmt) ->
     let ncols = Array.length colwidths in
     let nrows = Array.length fmt.(0) in
     for c = 0 to ncols - 1 do
       for r = 0 to nrows - 1 do
	 let s = fmt.(c).(r) in
	 fmt.(c).(r) <- Printf.sprintf "%- *s" (colwidths.(c)) s ;
	 assert (String.length fmt.(c).(r) = colwidths.(c)) ;
       done ;
     done ;
     fmt

let output ?(width=80) oc l =
  let fd = Unix.descr_of_out_channel oc in
  if not (Unix.isatty fd) then
    List.iter (fun s -> print_string s ; print_newline()) l
  else
    let fmt = format ~width l in
    let nrows = Array.length fmt.(0) in
    let ncols = Array.length fmt in
    for r = 0 to nrows - 1 do
      for c = 0 to ncols - 1 do
	print_string fmt.(c).(r)
      done ;
      print_newline()
    done

end

let app_i f = 
 let rec app_i_rec i = function
     [] -> () | x::l -> let _ = f i x in app_i_rec (i+1) l
 in app_i_rec
 

let invoked_with ?flag cmdna =
  let variant_names = [cmdna; cmdna^".byte"; cmdna^".native"; cmdna^".opt"] in

  let argv = Array.to_list Sys.argv in
  let path = Pcre.split ~rex:(Pcre.regexp "/") (car argv) in
  let fname, _ = sep_last path in

  List.exists ((=) fname) variant_names &&
  match flag with None -> true | Some flag ->
    let flag' = "-"^flag in
    let flag'' = "--"^flag in
    List.exists ((=) flag') (cdr argv) ||
      List.exists ((=) flag'') (cdr argv)

let split l =
  let rec srec al bl = function
      (a,b)::tl -> srec (a::al) (b::bl) tl
    | [] -> (List.rev al, List.rev bl)
  in srec [] [] l

let split_option = function
  | None -> (None, None)
  | Some (a, b) -> (Some a, Some b)

let combine l1 l2 =
  let rec crec acc = function
    | [],[] -> List.rev acc
    | h1::t1, h2::t2 -> crec ((h1,h2)::acc) (t1,t2)
    | _ -> invalid_arg "combine"
  in crec [] (l1,l2)

let combine3 l1 l2 l3 =
  let rec crec acc = function
    | [],[],[] -> List.rev acc
    | h1::t1, h2::t2, h3::t3 -> crec ((h1,h2,h3)::acc) (t1,t2,t3)
    | _ -> invalid_arg "combine3"
  in crec [] (l1,l2,l3)

let slice_string ?(spos=0) ?epos s =
  let epos = match epos with
      Some n -> n
    | None -> String.length s in
  if spos > epos then invalid_arg "slice_string" ;
  if spos > String.length s then invalid_arg "slice_string" ;
  if (spos < 0) then invalid_arg "slice_string" ;
  if (epos < 0) then invalid_arg "slice_string" ;
  String.sub s spos (epos - spos)

let pred_or pl x = List.exists (fun p -> p x) pl
