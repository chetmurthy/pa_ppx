
open Std

type ufrep = {mutable rep: int; mutable siz: int}

type 'a t = {a2n: ('a, int) Hashtbl.t;
			 n2a: 'a array;
			 reps: ufrep array}

let mk l =
  let nelts = List.length l in
  let v = {a2n=Hashtbl.create 23;
		   n2a=Array.of_list l;
		   reps = Array.init nelts (fun i -> {rep=i;siz=1})}
  in app_i (fun i a -> Hashtbl.add v.a2n a i) 0 l;
	v

let findaux v n =
  let rec findrec n =
	if v.reps.(n).rep = n then n
	else
	  (v.reps.(n).rep <- v.reps.(v.reps.(n).rep).rep;
	   findrec v.reps.(n).rep)

  in findrec n

let a2n v a = Hashtbl.find v.a2n a

let n2a v n = v.n2a.(n)

let find v a = n2a v (findaux v (a2n v a))

let union v a1 a2 =
  let n1 = a2n v a1 and
	n2 = a2n v a2 in
  let n1rep = findaux v n1 and
	n2rep = findaux v n2 in
	if (n1rep <> n2rep) then
	  (if (v.reps.(n1rep).siz < v.reps.(n2rep).siz) then
		 (v.reps.(n1rep).rep <- n2rep; v.reps.(n2rep).siz <- v.reps.(n2rep).siz + v.reps.(n1rep).siz)
	   else
		 (v.reps.(n2rep).rep <- n1rep; v.reps.(n1rep).siz <- v.reps.(n1rep).siz + v.reps.(n2rep).siz))
