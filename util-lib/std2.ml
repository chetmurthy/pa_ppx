
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
