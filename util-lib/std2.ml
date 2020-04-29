
open Coll

let hash_uniq l =
  let s = MHS.mk 23 in
  let rec unirec acc = function
  [] -> List.rev acc
    | h::t ->
       if MHS.mem h s then unirec acc t
       else (
	 MHS.add h s;
	 unirec (h::acc) t
       )
  in unirec [] l

let hash_union l1 l2 =
  let s = MHS.mk 23 in
  let rec urec acc = function
      [] -> acc
    | h::t -> if not (MHS.mem h s) then
        (MHS.add h s ; urec (h::acc) t)
      else urec acc t in
  let acc = urec [] l1 in
  let acc = urec acc l2 in
  List.rev acc

let hash_list_repeats l =
  let seen = MHS.mk 23 in
  let reported = MHS.mk 23 in
  let rec unirec acc = function
  [] -> List.rev acc
    | h::t ->
       if MHS.mem h seen then begin
         if not (MHS.mem h reported) then begin
           MHS.add h reported ;
           unirec (h::acc) t
         end
         else
           unirec acc t
       end
       else (
	 MHS.add h seen;
	 unirec acc t
       )
  in unirec [] l
