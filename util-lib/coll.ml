(* Copyright 2016 Chetan Murthy *)

open Std
module type FUNSET =
  sig
    type 'a init_t
	type 'a t
    val mk : 'a init_t -> 'a t
    val subset : 'a t -> 'a t -> bool
    val add : 'a -> 'a t -> 'a t
    val rmv : 'a -> 'a t -> 'a t
    val map : ('a -> 'b) -> 'a t -> 'b init_t -> 'b t
    val app : ('a -> unit) -> 'a t -> unit
    val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
    val filter : ('a -> bool) -> 'a t -> 'a t
    val try_find : ('a -> 'b) -> 'a t -> 'b
    val mem : 'a -> 'a t -> bool
    val equal : 'a t -> 'a t -> bool
    val toList : 'a t-> 'a list
    val ofList : 'a list -> 'a init_t -> 'a t
    val for_all : ('a -> bool) -> 'a t -> bool
    val exists : ('a -> bool) -> 'a t -> bool
    val empty : 'a t -> bool
    val size : 'a t -> int
    val clear : 'a t -> 'a t
  end

module ListSet : (FUNSET with type 'a t = 'a list and type 'a init_t = unit) =
struct
    type 'a t    = 'a list
	type 'a init_t = unit
	let mk () = []
    let subset   = subset
    let add      = add_set
    let rmv      = rmv_set
    let map f s initv = List.map f s
	let app = List.iter
	let fold = List.fold_left 
	let try_find = try_find
	let filter p l = filter p l
    let mem      = List.mem
    let equal    = same_members
	let toList l = l
	let ofList l () = uniquize l
    let for_all  = List.for_all
	let exists = List.exists
	let empty = function [] -> true | _::_ -> false
	let size l = List.length l
	let clear l = []
end

module LS = ListSet

module type MUTSET =
  sig
    type 'a init_t
    type 'a t
    val mk : 'a init_t -> 'a t
    val subset : 'a t -> 'a t -> bool
    val add : 'a -> 'a t -> unit
    val rmv : 'a -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b init_t -> 'b t
    val app : ('a -> unit) -> 'a t -> unit
    val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
    val try_find : ('a -> 'b) -> 'a t -> 'b
    val mem : 'a -> 'a t -> bool
    val equal : 'a t -> 'a t -> bool
    val toList : 'a t -> 'a list
    val ofList : 'a list -> 'a init_t -> 'a t
    val for_all : ('a -> bool) -> 'a t -> bool
    val exists : ('a -> bool) -> 'a t -> bool
    val empty : 'a t -> bool
    val size : 'a t -> int
    val clear : 'a t -> unit
  end

module MutFUNSET (F : FUNSET) : (MUTSET with type 'a init_t = 'a F.init_t) =
struct
  type 'a init_t = 'a F.init_t
  type 'a t = 'a F.t ref
  let mk initv = ref (F.mk initv)
  let subset s1 s2 = F.subset !s1 !s2
  let add x s = s := F.add x !s
  let rmv x s = s := F.rmv x !s
  let map f s initv = ref(F.map f !s initv)
  let app f s = F.app f !s
  let fold f initv s = F.fold f initv !s
  let try_find f s = F.try_find f !s
  let mem x s = F.mem x !s
  let equal s1 s2 = F.equal !s1 !s2
  let toList s = F.toList !s
  let ofList l initv = ref(F.ofList l initv)
  let for_all f s = F.for_all f !s
  let exists f s = F.for_all f !s
  let empty s = F.empty !s
  let size s = F.size !s
  let clear s = s := F.clear !s
end

module MLS = MutFUNSET(LS)

module HashSet : (MUTSET with type 'a init_t = int) =
  struct
	type 'a init_t = int
	type 'a t = ('a, unit) Hashtbl.t * int ref
	let mk n = (Hashtbl.create n, ref 0)
	let mem k (t,_) = Hashtbl.mem t k
	let rmv k (t,n) =
	  if not(mem k (t,n)) then ()
	  else (Hashtbl.remove t k; decr n)

	let add k (t,n) =
	  if not(mem k (t,n)) then (Hashtbl.add t k (); incr n) else ()
	exception Caught

	let app f (t,_) = Hashtbl.iter (fun k _ -> f k) t

	let fold f initv t =
	  let acc = ref initv in
		app (fun v -> acc := (f !acc v)) t;
		!acc

	let for_all pred t =
	  try app (fun k -> if not(pred k) then raise Caught else ()) t; true
	  with Caught -> false

	let exists pred t =
	  try app (fun k -> if pred k then raise Caught else ()) t; false
	   with Caught -> true

	let subset t1 t2 =
	  for_all (fun k1 -> mem k1 t2) t1

	let equal t1 t2 = subset t1 t2 && subset t2 t1

	let toList t =
	  let l = ref [] in
		app (fun k -> l := k :: !l) t;
		!l

	let ofList l init =
	  let t = mk init in
		List.iter (fun k -> add k t) l;
		t

	let try_find f t = try_find f (toList t)

	let map f t1 init2 =
	  let t2 = mk init2 in
		app (fun k -> add (f k) t2) t1;
		t2

	let empty (_,n) = !n = 0

	let size (_,n) = !n
	let clear (t,n) =
	  Hashtbl.reset t ;
	  n := 0
  end

module MHS = HashSet

module type FUNMAP =
sig
  type ('a,'b) init_t
  type ('a,'b) t
  val mk : ('a,'b) init_t -> ('a,'b) t
  val map : ('a,'b) t -> 'a -> 'b
  val dom : ('a,'b) t -> 'a list
  val rng : ('a,'b) t -> 'b list
  val in_dom : ('a,'b) t -> 'a -> bool
  val in_rng : ('a,'b) t -> 'b -> bool
  val inv : ('a,'b) t -> 'b -> 'a list
  val add : ('a,'b) t -> 'a * 'b -> ('a,'b) t
  val rmv : ('a,'b) t -> 'a -> ('a,'b) t
  val remap : ('a,'b) t -> 'a -> 'b -> ('a,'b) t
  val app : ('a -> 'c -> unit) -> ('a,'c) t -> unit
  val fold : ('a -> ('b * 'c) -> 'a) -> 'a -> ('b,'c) t -> 'a
  val try_find : (('a * 'b) -> 'c) -> ('a,'b) t -> 'c
  val filter : (('a * 'b) -> bool) -> ('a,'b) t -> ('a,'b) t
  val toList : ('a,'b) t -> ('a * 'b) list
  val ofList : ('a,'b) init_t -> ('a * 'b) list -> ('a,'b) t
  val for_all : ('a -> 'b -> bool) -> ('a,'b) t -> bool
  val exists : ('a -> 'b -> bool) -> ('a,'b) t -> bool
  val size : ('a,'b) t -> int
  val empty : ('a, 'b) t -> bool
  val clear : ('a, 'b) t -> ('a, 'b) t
end

module type ORDERED_FUNMAP =
sig
  include FUNMAP
  val min : ('a, 'b) t -> 'a
  val max : ('a, 'b) t -> 'a
end

module type MUTMAP =
sig
  type ('a,'b) init_t
  type ('a,'b) t
  val mk : ('a,'b) init_t -> ('a,'b) t
  val map : ('a,'b) t -> 'a -> 'b
  val dom : ('a,'b) t -> 'a list
  val rng : ('a,'b) t -> 'b list
  val in_dom : ('a,'b) t -> 'a -> bool
  val in_rng : ('a,'b) t -> 'b -> bool
  val inv : ('a,'b) t -> 'b -> 'a list
  val add : ('a,'b) t -> 'a * 'b -> unit
  val rmv : ('a,'b) t -> 'a -> unit
  val remap : ('a,'b) t -> 'a -> 'b -> unit
  val app : ('a -> 'c -> unit) -> ('a,'c) t -> unit
  val fold : ('a -> ('b * 'c) -> 'a) -> 'a -> ('b,'c) t -> 'a
  val try_find : (('a * 'b) -> 'c) -> ('a,'b) t -> 'c
  val toList : ('a,'b) t -> ('a * 'b) list
  val ofList : ('a,'b) init_t -> ('a * 'b) list -> ('a,'b) t
  val for_all : ('a -> 'b -> bool) -> ('a,'b) t -> bool
  val exists : ('a -> 'b -> bool) -> ('a,'b) t -> bool
  val size : ('a,'b) t -> int
  val empty : ('a, 'b) t -> bool
  val clear : ('a, 'b) t -> unit
end

module type ORDERED_MUTMAP =
sig
  include MUTMAP
  val min : ('a, 'b) t -> 'a
  val max : ('a, 'b) t -> 'a
end

module RawMutFUNMAP (F : FUNMAP) =
struct
  type ('a,'b) init_t = ('a,'b) F.init_t
  type ('a,'b) t = ('a,'b) F.t ref

  let mk initv = ref (F.mk initv)
  let map m k = F.map !m k
  let dom m = F.dom !m
  let rng m = F.rng !m
  let in_dom m k = F.in_dom !m k
  let in_rng m k = F.in_rng !m k
  let inv m k = F.inv !m k
  let add m (k,v) = m := F.add !m (k,v)
  let rmv m k = m := F.rmv !m k
  let remap m k v = m := F.remap !m k v
  let app f m = F.app f !m
  let toList m = F.toList !m

  let ofList initv l =
    let m = mk initv in
      List.iter (add m) l ;
      m

  let for_all f m = F.for_all f !m
  let exists f m = F.exists f !m
  let size m = F.size !m
  let empty m = F.empty !m
  let clear m = m := F.clear !m

  let fold f initv m = F.fold f initv !m
  let try_find f m = F.try_find f !m
end

module MutFUNMAP (F : FUNMAP) : (MUTMAP with type ('a,'b) init_t = ('a,'b) F.init_t) = RawMutFUNMAP(F)


module MutORDERED_FUNMAP (F : ORDERED_FUNMAP) : (ORDERED_MUTMAP with type ('a,'b) init_t = ('a,'b) F.init_t) =
struct
  module MM = RawMutFUNMAP(F)
  type ('a,'b) t = ('a, 'b) MM.t
  type ('a,'b) init_t = ('a, 'b) MM.init_t

  let mk = MM.mk
  let map = MM.map
  let dom = MM.dom
  let rng = MM.rng
  let in_dom = MM.in_dom
  let in_rng = MM.in_rng
  let inv = MM.inv
  let add = MM.add
  let rmv = MM.rmv
  let remap = MM.remap
  let app = MM.app
  let toList = MM.toList

  let ofList = MM.ofList
  let for_all = MM.for_all
  let exists = MM.exists
  let size = MM.size
  let min m = F.min !m
  let max m = F.max !m
  let empty m = F.empty !m
  let clear m = m := F.clear !m

  let fold f initv m = F.fold f initv !m
  let try_find f m = F.try_find f !m
end

module RawListMap =
struct
  type ('a,'b) t = ('a *'b) list
  type ('a,'b) init_t = unit

let rec delete n = function
    [] -> invalid_arg "Listmap.delete"
  | (a,b)::t ->
      if n = a then
        t
      else
        ((a,b)::(delete n t))

let mk() = []
let map l a = List.assoc a l

let dom l = List.map fst l

let rng l = List.map snd l

let in_dom l a = 
  let rec in_dom_a = function
      [] -> false
    | ((a',_)::l) -> (a = a') || in_dom_a l
  in in_dom_a l 


let in_rng l b = 
  let rec in_rng_b = function
      [] -> false
    | ((_,b')::l) -> (b = b') || in_rng_b l
  in in_rng_b l


let inv l b = List.map fst (filter (fun (a',b') -> b' = b) l)

let add l (a,b) =
  if in_dom l a then invalid_arg "Listmap.add"
  else ((a,b)::l)

let rmv l a =
  if in_dom l a then
    delete a l
  else
    raise Not_found

let remap l a b =
  let rec aux = function
      ((k,v)::t) ->
    	if k = a then (a,b)::t
    	else (k,v)::(aux t)
    | _ -> invalid_arg "Listmap.remap"
  in
    aux l

let app f l = List.iter (fun (a,b) -> f a b) l

let fold f initv l = List.fold_left f initv l

let try_find f l = try_find f l

let filter p l = filter p l

let toList l = l
let ofList initv l =
  if not (distinct (List.map fst l)) then invalid_arg "Listmap.ofList"
  else l

let exists f l =  
  let rec fall = function 
      [] -> true
    | ((a,b)::x) -> (f a b) || fall x
  in  fall l

let for_all f l =  
  let rec fall = function 
      [] -> true
    | ((a,b)::x) -> (f a b) && fall x
  in  fall l

let size l = List.length l
let empty l = l=[]
let clear l = []
end

module ListMap : (FUNMAP with type ('a,'b) t = ('a * 'b) list and type ('a,'b) init_t = unit) = RawListMap

module OrderedListMap : (ORDERED_FUNMAP with type ('a,'b) t = ('a *'b) list and type ('a, 'b) init_t = unit) = struct
  type ('a, 'b) t = ('a, 'b) RawListMap.t
  type ('a, 'b) init_t = ('a, 'b) RawListMap.init_t

  let mk = RawListMap.mk

  let map = RawListMap.map
  let dom = RawListMap.dom
  let rng = RawListMap.rng
  let in_dom = RawListMap.in_dom
  let in_rng = RawListMap.in_rng
  let rmv = RawListMap.rmv
  let app = RawListMap.app
  let fold = RawListMap.fold
  let try_find = RawListMap.try_find
  let filter = RawListMap.filter
  let toList = RawListMap.toList
  let for_all = RawListMap.for_all
  let exists = RawListMap.exists
  let size = RawListMap.size
  let inv = RawListMap.inv
  let remap = RawListMap.remap
  let empty = RawListMap.empty
  let clear = RawListMap.clear

  let add l (k,v) =
    let rec addrec = function
	(k',_ as p)::l when k' < k -> p::(addrec l)
      | (k',_)::_ when k' = k -> invalid_arg "OrderedMap.add"
      | l -> (k,v)::l in
      addrec l

  let ofList initv l = List.fold_left add (mk initv) l

  let min = function
      (k,_)::_ -> k
    | _ -> failwith "OrderedMap.min"

  let max l = fst(last l)

end


module LM = ListMap
module OLM = OrderedListMap

module MLM = MutFUNMAP(LM)
module MOLM = MutORDERED_FUNMAP(OLM)

module type FREEZABLE_MUTMAP =
  sig
    include MUTMAP
    type ('a,'b) frozen_t
    val freeze : ('a, 'b) t -> ('a,'b) frozen_t
    val unfreeze : ('a,'b) t -> ('a,'b) frozen_t -> unit
  end

module LazyCache (M : MUTMAP) =
struct
  type ('a,'b) t =
      { mutable it : ('a,'b option) M.t option ;
	init : ('a,'b option) M.init_t }
  let mk initv =
    { init = initv ;
      it = None }

  let mkFrom c = { init = c.init ; it = None }

  let fill c fmf k =
    if c.it = None then c.it <- Some (M.mk c.init);
    match c.it with
	None -> assert false
      | Some mm ->
	  assert (not (M.in_dom mm k));
	  try
	    let v = fmf k
	    in M.add mm (k,Some v)
	  with Not_found ->
	    M.add mm (k, None)

  let filled c k =
    match c.it with None -> true
      | Some mm ->
	  not(M.in_dom mm k)

  let lookup c k =
    match c.it with
	None -> raise Not_found
      | Some mm -> M.map mm k

  let flush c k =
    match c.it with
	None -> ()
      | Some mm -> M.rmv mm k

  let flush_all c = c.it <- None

end

module FastFUNMAP (F : FUNMAP) (M : MUTMAP) : (FUNMAP with type ('a,'b) init_t = ('a, 'b option) M.init_t * ('a, 'b) F.init_t) =
struct

  module Cache = LazyCache(M)

  type ('a,'b) init_t = ('a, 'b option) M.init_t * ('a, 'b) F.init_t
  type ('a,'b) t =
      { c : ('a,'b) Cache.t ;
	fm : ('a, 'b) F.t }

  let mk (a,b) =
    { c = Cache.mk a ;
      fm = F.mk b }

  let map m k =
    match
      try Cache.lookup m.c k
      with Not_found ->
	Cache.fill m.c (F.map m.fm) k; Cache.lookup m.c k
      with
	  None -> raise Not_found
	| Some v -> v

  let dom m = F.dom m.fm
  let rng m = F.rng m.fm
  let in_dom m = F.in_dom m.fm
  let in_rng m = F.in_rng m.fm
  let inv m = F.inv m.fm

  let add m (k,v) =
    let fm = F.add m.fm (k,v)
    in
      assert (not(Cache.filled m.c k));
      { m with fm = fm ; c = Cache.mkFrom m.c }

  let rmv m k =
    let fm = F.rmv m.fm k
    in
      { m with fm = fm ; c = Cache.mkFrom m.c }

  let remap m k v =
    let fm = F.remap m.fm k v
    in
      { m with fm = fm ; c = Cache.mkFrom m.c }

  let app f m = F.app f m.fm
  let toList m = F.toList m.fm
  let ofList initv l = List.fold_left add (mk initv) l
  let for_all f m = F.for_all f m.fm
  let exists f m = F.exists f m.fm
  let size m = F.size m.fm
  let filter f m =
    { m with fm = F.filter f m.fm ; c = Cache.mkFrom m.c }
  let fold f initv m =
    F.fold f initv m.fm
  let try_find f m = F.try_find f m.fm
  let empty m = F.empty m.fm
  let clear m =
    let m = { m with fm = F.clear m.fm ; c = Cache.mkFrom m.c } in
      Cache.flush_all m.c ;
      m

end

module FreezableMUTMAP (F : FUNMAP) (M : MUTMAP) : (FREEZABLE_MUTMAP with type ('a,'b) init_t = ('a,'b option) M.init_t * ('a,'b) F.init_t) =
struct
  module Cache = LazyCache(M)

  type ('a,'b) init_t = ('a,'b option) M.init_t * ('a,'b) F.init_t

  type ('a,'b) t =
      { c : ('a,'b) Cache.t ;
	mutable fm : ('a,'b) F.t }

  let mk (a,b) =
    { c = Cache.mk a ;
      fm = F.mk b }

  let map m k =
    match
      try Cache.lookup m.c k
      with Not_found ->
	Cache.fill m.c (F.map m.fm) k ; Cache.lookup m.c k
      with
	  None -> raise Not_found
	| Some v -> v

  let dom m = F.dom m.fm
  let rng m = F.rng m.fm
  let in_dom m = F.in_dom m.fm
  let in_rng m = F.in_rng m.fm
  let inv m = F.inv m.fm

  let add m (k,v) =
    let fm' = F.add m.fm (k,v)
    in
      assert (not(Cache.filled m.c k));
      m.fm <- fm'

  let rmv m k =
    let fm' = F.rmv m.fm k
    in
      Cache.flush m.c k;
      m.fm <- fm'

  let remap m k v =
    let fm' = F.remap m.fm k v in
      Cache.flush m.c k ;
      m.fm <- fm'

  let app f m = F.app f m.fm
  let toList m = F.toList m.fm
  let ofList initv l =
    let m = mk initv in
      List.iter (add m) l ;
      m
  let for_all f m = F.for_all f m.fm
  let exists f m = F.exists f m.fm
  let size m = F.size m.fm
  let empty m = F.empty m.fm
  let clear m =
    Cache.flush_all m.c ;
    m.fm <- F.clear m.fm

  type ('a,'b) frozen_t = ('a,'b) F.t

  let freeze m = m.fm
  let unfreeze m fm =
    Cache.flush_all m.c ;
    m.fm <- fm

  let fold f initv m = F.fold f initv m.fm
  let try_find f m = F.try_find f m.fm

end

module type FREEZABLE_MUTSET =
  sig
    include MUTSET
    type 'a frozen_t
    val freeze : 'a t -> 'a frozen_t
    val unfreeze : 'a t -> 'a frozen_t -> unit
  end

module FreezableMUTSET (F : FUNSET) (M : MUTMAP) : (FREEZABLE_MUTSET with type 'a init_t = 'a F.init_t * ('a, unit option) M.init_t) =
struct
  module Cache = LazyCache(M)
  type 'a init_t = 'a F.init_t * ('a, unit option) M.init_t
  type 'a t =
      { mutable fs : 'a F.t ;
	finit : 'a F.init_t ;
	c : ('a, unit) Cache.t }

  let mk (a,b) =
      { fs = F.mk a ;
	finit = a ;
	c = Cache.mk b }

  let subset u v = F.subset u.fs v.fs
  let add x s =
    let fs' = F.add x s.fs in
      Cache.flush s.c x;
      s.fs <- fs'

  let rmv x s =
    let fs' = F.rmv x s.fs in
      Cache.flush s.c x;
      s.fs <- fs'

  let map f s (a,b) =
    let fs' = F.map f s.fs a in
      { fs = fs' ; c = Cache.mk b ; finit = a }

  let app f s = F.app f s.fs
  let fold f initv s = F.fold f initv s.fs
  let try_find f s = F.try_find f s.fs

  let mem x s =
    match
      try Cache.lookup s.c x
      with Not_found ->
	Cache.fill s.c (fun x -> if F.mem x s.fs then () else raise Not_found) x ;
	Cache.lookup s.c x
      with
	  None -> false
	| Some () -> true

  let equal s1 s2 = F.equal s1.fs s2.fs

  let toList s = F.toList s.fs
  let ofList l (a,b) =
    { fs = F.ofList l a; 
      finit = a ;
      c = Cache.mk b }

  let for_all f s = F.for_all f s.fs
  let exists f s = F.exists f s.fs

  let empty s = F.empty s.fs
  let size s = F.size s.fs
  let clear s =
    F.clear s.fs ;
    Cache.flush_all s.c

  type 'a frozen_t = 'a F.t
  let freeze s = s.fs
  let unfreeze s fs =
    s.fs <- fs ;
    Cache.flush_all s.c

end


module type FOFUNMAP =
sig
  type dom
  type 'b init_t
  type 'b t
  val mk : 'b init_t -> 'b t
  val map : 'b t -> dom -> 'b
  val dom : 'b t -> dom list
  val rng : 'b t -> 'b list
  val in_dom : 'b t -> dom -> bool
  val in_rng : 'b t -> 'b -> bool
  val inv : 'b t -> 'b -> dom list
  val add : 'b t -> dom * 'b -> 'b t
  val rmv : 'b t -> dom -> 'b t
  val remap : 'b t -> dom -> 'b -> 'b t
  val app : (dom -> 'c -> unit) -> ('c) t -> unit
  val fold : ('a -> (dom * 'c) -> 'a) -> 'a -> 'c t -> 'a
  val try_find : ((dom * 'b) -> 'c) -> 'b t -> 'c
  val filter : ((dom * 'b) -> bool) -> 'b t -> 'b t
  val toList : 'b t -> (dom * 'b) list
  val ofList : 'b init_t -> (dom * 'b) list -> 'b t
  val for_all : (dom -> 'b -> bool) -> 'b t -> bool
  val exists : (dom -> 'b -> bool) -> 'b t -> bool
  val size : 'b t -> int
  val empty : ( 'b) t -> bool
  val clear : ( 'b) t -> 'b t
end

module type ORDERED_FOFUNMAP =
sig
  include FOFUNMAP
  val min : 'b t -> dom
  val max : 'b t -> dom
end

module type FOMUTMAP =
  sig
    type dom
    type 'rng init_t
    type 'rng t
    val mk : 'rng init_t -> 'rng t
    val map : 'rng t -> dom -> 'rng
    val dom : 'rng t -> dom list
    val rng : 'rng t -> 'rng list
    val in_dom : 'rng t -> dom -> bool
    val in_rng : 'rng t -> 'rng -> bool
    val inv : 'rng t -> 'rng -> dom list
    val add : 'rng t -> dom * 'rng -> unit
    val rmv : 'rng t -> dom -> unit
    val remap : 'rng t -> dom -> 'rng -> unit
    val app : (dom -> 'rng -> unit) -> 'rng t -> unit
    val fold : ('a -> (dom * 'rng) -> 'a) -> 'a -> 'rng t -> 'a
    val try_find : ((dom * 'rng) -> 'c) -> 'rng t -> 'c
    val toList : 'rng t -> (dom * 'rng) list
    val for_all : (dom -> 'rng -> bool) -> 'rng t -> bool
    val exists : (dom -> 'rng -> bool) -> 'rng t -> bool
    val size : 'rng t -> int
    val empty : 'rng t -> bool
    val clear : 'rng t -> unit
  end

module type ORDERED_FOMUTMAP =
sig
  include FOMUTMAP
  val min : 'b t -> dom
  val max : 'b t -> dom
end


module RawMutFOFUNMAP (F : FOFUNMAP) =
struct
  type dom = F.dom
  type 'b init_t = 'b F.init_t
  type 'b t = 'b F.t ref

  let mk initv = ref (F.mk initv)
  let map m k = F.map !m k
  let dom m = F.dom !m
  let rng m = F.rng !m
  let in_dom m k = F.in_dom !m k
  let in_rng m k = F.in_rng !m k
  let inv m k = F.inv !m k
  let add m (k,v) = m := F.add !m (k,v)
  let rmv m k = m := F.rmv !m k
  let remap m k v = m := F.remap !m k v
  let app f m = F.app f !m
  let toList m = F.toList !m

  let ofList initv l =
    let m = mk initv in
      List.iter (add m) l ;
      m

  let for_all f m = F.for_all f !m
  let exists f m = F.exists f !m
  let size m = F.size !m
  let empty m = F.empty !m
  let clear m = m := F.clear !m

  let fold f initv m = F.fold f initv !m
  let try_find f m = F.try_find f !m
end

module MutFOFUNMAP (F : FOFUNMAP) : (FOMUTMAP with type 'b init_t = 'b F.init_t and type dom = F.dom) = RawMutFOFUNMAP(F)


module MutORDERED_FOFUNMAP (F : ORDERED_FOFUNMAP) : (ORDERED_FOMUTMAP with type 'b init_t = 'b F.init_t and type dom = F.dom) =
struct
  module MM = RawMutFOFUNMAP(F)
  type dom = MM.dom
  type 'b t = 'b MM.t
  type 'b init_t = 'b MM.init_t

  let mk = MM.mk
  let map = MM.map
  let dom = MM.dom
  let rng = MM.rng
  let in_dom = MM.in_dom
  let in_rng = MM.in_rng
  let inv = MM.inv
  let add = MM.add
  let rmv = MM.rmv
  let remap = MM.remap
  let app = MM.app
  let toList = MM.toList

  let ofList = MM.ofList
  let for_all = MM.for_all
  let exists = MM.exists
  let size = MM.size
  let min m = F.min !m
  let max m = F.max !m
  let empty m = F.empty !m
  let clear m = m := F.clear !m

  let fold f initv m = F.fold f initv !m
  let try_find f m = F.try_find f !m
end


module Map2ORDERED_FOFUNMAP(Ord : Map.OrderedType) : (ORDERED_FOFUNMAP with type dom = Ord.t) = struct
  module M = Map.Make(Ord)
  type dom = Ord.t

  type 'a init_t = unit
  type 'a t = 'a M.t

  let mk () = M.empty
  let map m k = M.find k m
  let dom m = List.map fst (M.bindings m)
  let rng m = List.map snd (M.bindings m)
  let in_dom m k = M.mem k m
  let in_rng m v = LS.mem v (rng m)

  let  inv m v0 =
    M.fold
      (fun k v acc -> if v = v0 then k::acc else acc)
      m []

  let add m (k,v) = M.add k v m
  let rmv m k = M.remove k m
  let remap m k v =
    if in_dom m k then
      add (rmv m k) (k,v)
    else add m (k,v)

  let app = M.iter
  let fold f initv m =
    M.fold
      (fun k v acc -> f acc (k,v))
      m initv

  let toList = M.bindings
  let ofList initv l = List.fold_left add (mk initv) l

  let try_find f m =
    let rv = ref None in
      (try
	 M.iter
	   (fun k v ->
	      (try rv := Some(f(k,v))
	       with Failure _ -> ()) ;
	      if !rv <> None then failwith "caught")
	   m 
       with Failure _ -> ()) ;
      match !rv with
	  Some v -> v
	| None -> failwith "try_find"

  let filter pred m =
    fold
      (fun acc (k,v) ->
	 if pred (k,v) then add acc (k,v) else acc)
      (mk()) m

  let for_all f m = List.for_all (fun (a,b) -> f a b) (toList m)
  let exists f m = List.exists (fun (a,b) -> f a b) (toList m)
  let size m = fold (fun n _ -> n+1) 0 m
  let empty m = m = M.empty
  let clear m = mk()

  let min m = fst(M.min_binding m)
  let max m = fst(M.max_binding m)


end

module type FullFOMUTMAP =
  sig
    type dom
    type rng
    type init_t
    type t
	  val mk : init_t -> t
	  val map : t -> dom -> rng
	  val dom : t -> dom list
	  val rng : t -> rng list
	  val in_dom : t -> dom -> bool
	  val in_rng : t -> rng -> bool
	  val inv : t -> rng -> dom list
	  val add : t -> dom * rng -> unit
	  val rmv : t -> dom -> unit
	  val remap : t -> dom -> rng -> unit
	  val app : (dom -> rng -> unit) -> t -> unit
	  val fold : ('a -> (dom * rng) -> 'a) -> 'a -> t -> 'a
	  val try_find : ((dom * rng) -> 'c) -> t -> 'c
	  val toList : t -> (dom * rng) list
	  val for_all : (dom -> rng -> bool) -> t -> bool
	  val exists : (dom -> rng -> bool) -> t -> bool
	  val size : t -> int
	  val empty : t -> bool
	  val clear : t -> unit
  end

module MHM : (MUTMAP with type ('a,'b) t = ('a,'b) Hashtbl.t * int ref and type ('a,'b) init_t = int) =
struct
  type ('a,'b) init_t = int
  type ('a,'b) t = ('a,'b) Hashtbl.t * int ref

  let mk n = (Hashtbl.create n, ref 0)
  let map (t,_) k = Hashtbl.find t k
  let app f (t,_) = Hashtbl.iter f t

  let dom t =
    let l = ref []
    in app (fun k _ -> l := k :: !l) t;
      !l
	
  let rng t =
    let l = ref []
    in app (fun _ v -> l := v :: !l) t;
      !l
	
  let in_dom (t,_) k = Hashtbl.mem t k

  exception Caught

  let for_all pred t =
    try app (fun k v -> if not(pred k v) then raise Caught else ()) t; true
    with Caught -> false

  let exists pred t =
    try app (fun k v -> if (pred k v) then raise Caught else ()) t; false
    with Caught -> true

  let in_rng t v = exists (fun _ v' -> v = v') t

  let inv t v =
    let l = ref []
    in app (fun k v' -> if v = v' then l := k :: !l) t;
      !l
	
  let add ((t,n) as p) (k,v) =
    if in_dom p k then failwith "MHM.add: already mapped"
    else (Hashtbl.add t k v; incr n)

  let rmv ((t,n) as p) k =
    if in_dom p k then (Hashtbl.remove t k; decr n)

  let remap t k v =
    rmv t k; add t (k,v)

  let toList t =
    let l = ref []
    in app (fun k v -> l := (k,v):: !l) t;
      !l
	
  let ofList initv l =
    let m = mk initv in
      List.iter (add m) l ;
      m
	
  let size (_,n) = !n
  let empty m = 0 = size m
  let clear (t,n) = Hashtbl.clear t ; n := 0

  let fold f initv (t,_) = Hashtbl.fold (fun k v c -> f c (k,v)) t initv
  let try_find f m =
    let rv = ref None in
      app (fun k v -> try rv := Some (f (k,v)) with Failure _ -> ()) m ;
      match !rv with
	  None -> failwith "MHM.try_find"
	| Some v -> v
end

module FOMHM (T : sig type dom val equal : dom -> dom -> bool val hash : dom -> int end) : (FOMUTMAP with type dom = T.dom and type 'a init_t = int) =
struct
  type dom = T.dom

  module Hashtbl = Hashtbl.Make(struct type t = dom let equal = T.equal let hash = T.hash end)

  type 'a init_t = int
  type 'rng t = 'rng Hashtbl.t * int ref

  let mk n = (Hashtbl.create n, ref 0)
  let map (t,_) k = Hashtbl.find t k
  let app f (t,_) = Hashtbl.iter f t

  let dom t =
    let l = ref []
    in app (fun k _ -> l := k :: !l) t;
		!l

  let rng t =
    let l = ref []
    in app (fun _ v -> l := v :: !l) t;
      !l

  let in_dom (t,_) k = Hashtbl.mem t k

  exception Caught

  let for_all pred t =
    try app (fun k v -> if not(pred k v) then raise Caught else ()) t; true
    with Caught -> false

  let exists pred t =
    try app (fun k v -> if (pred k v) then raise Caught else ()) t; false
    with Caught -> true

  let in_rng t v = exists (fun _ v' -> v = v') t
	  
  let inv t v =
    let l = ref []
    in app (fun k v' -> if v = v' then l := k :: !l) t;
      !l

  let add ((t,n) as p) (k,v) =
    if in_dom p k then failwith "MHM.add: already mapped"
    else (Hashtbl.add t k v; incr n)

  let rmv ((t,n) as p) k =
    if in_dom p k then (Hashtbl.remove t k; decr n)

  let remap t k v =
    rmv t k; add t (k,v)

  let toList t =
    let l = ref []
    in app (fun k v -> l := (k,v):: !l) t;
      !l
  let size (_,n) = !n
  let empty m = 0 = size m
  let clear (t,_) = Hashtbl.clear t
    
  let fold f initv (t,_) = Hashtbl.fold (fun k v c -> f c (k,v)) t initv
  let try_find f m =
    let rv = ref None in
      app (fun k v -> try rv := Some (f (k,v)) with Failure _ -> ()) m ;
      match !rv with
	  None -> failwith "MHM.try_find"
	| Some v -> v
	    
end

module FullFOMHM (T : sig type dom type rng val equal : dom -> dom -> bool val hash : dom -> int end) : (FullFOMUTMAP with type dom = T.dom and type rng = T.rng and type init_t = int) =
  struct
    type dom = T.dom
    type rng = T.rng

    module FOMHM = FOMHM(struct type dom = T.dom let equal = T.equal let hash = T.hash end)

    type init_t = rng FOMHM.init_t
    type t = rng FOMHM.t

	let mk = FOMHM.mk
	let map = FOMHM.map
	let app = FOMHM.app
	let dom = FOMHM.dom
	let rng = FOMHM.rng
	let in_dom = FOMHM.in_dom
	let for_all = FOMHM.for_all
	let exists = FOMHM.for_all
	let in_rng = FOMHM.in_rng
	let inv = FOMHM.inv
	let add = FOMHM.add
	let rmv = FOMHM.rmv
	let remap = FOMHM.remap
	let toList = FOMHM.toList
	let size = FOMHM.size
	let empty = FOMHM.empty
	let clear = FOMHM.clear

	let fold = FOMHM.fold
	let try_find = FOMHM.try_find
  end

module MutBIJ (M: MUTMAP) : (MUTMAP with type ('a,'b) init_t = ('a,'b) M.init_t * ('b,'a) M.init_t) =
struct
  type ('a,'b) init_t = ('a,'b) M.init_t * ('b,'a) M.init_t
  type ('a,'b) t = ('a,'b) M.t * ('b,'a) M.t
  let mk (n,m) = (M.mk n, M.mk m)
  let map (lr,_) k = M.map lr k
  let app f (lr,_) = M.app f lr
  let dom (lr,_) = M.dom lr
  let rng (_,rl) = M.dom rl
  let in_dom (lr,_) k = M.in_dom lr k
  let in_rng (_,rl) v = M.in_dom rl v
  let for_all pred (lr,_) = M.for_all pred lr
  let exists pred (lr,_) = M.exists pred lr
  let inv (_,rl) v = [M.map rl v]

  let add (lr,rl) (k,v) =
    if M.in_dom lr k then failwith "MutBIJ.add: already in domain";
    if M.in_dom rl v then failwith "MutBIJ.add: already in range";
    M.add lr (k,v);
    M.add rl (v,k)
      
  let rmv (lr,rl) k =
    let v = M.map lr k in
      M.rmv lr k;
      M.rmv rl v
	
  let remap ((lr,rl) as t) k v =
    rmv t k;
    add t (k,v)
      
  let toList (lr,rl) =
    assert((List.length (M.toList lr)) = (List.length (M.toList rl)));
    M.toList lr
      
  let ofList initv l =
    let m = mk initv in
      List.iter (add m) l ;
      m
	
  let size (lr,rl) = M.size lr
  let empty (lr,rl) = M.empty lr
  let clear (lr,rl) =
    M.clear lr ;
    M.clear rl

  let fold f initv (lr,_) = M.fold f initv lr
  let try_find f (lr,_) = M.try_find f lr
end

module MHBIJ = MutBIJ(MHM)

module type FUNDEQ =
  sig
    type 'a t
    type 'a init_t
    val mk : 'a init_t -> 'a t

    val front : 'a t -> 'a option * 'a t
    val shift : 'a t -> ('a * 'a t)
    val pop_front : 'a t -> ('a * 'a t)
    val unshift : 'a -> 'a t -> 'a t
    val push_front : 'a -> 'a t -> 'a t

    val back : 'a t -> 'a t * 'a option
    val pop : 'a t -> ('a t * 'a)
    val push : 'a t -> 'a -> 'a t
    val pop_back : 'a t -> ('a t * 'a)
    val push_back : 'a t -> 'a -> 'a t

    val empty : 'a t -> bool
    val clear : 'a t -> 'a t
    val toList : 'a t -> 'a list
    val ofList : 'a list -> 'a init_t -> 'a t
    val first : 'a t -> 'a
    val last : 'a t -> 'a
    val for_all : ('a -> bool) -> 'a t -> bool
    val exists : ('a -> bool) -> 'a t -> bool
    val app : ('a -> unit) -> 'a t -> unit
  end

module type MUTDEQ =
  sig
    type 'a t
    type 'a init_t
    val mk : 'a init_t -> 'a t

    val front : 'a t -> 'a option
    val shift : 'a t -> 'a
    val unshift : 'a -> 'a t -> unit
    val pop_front : 'a t -> 'a
    val push_front : 'a -> 'a t -> unit

    val back : 'a t -> 'a option
    val pop : 'a t -> 'a
    val push : 'a t -> 'a -> unit
    val pop_back : 'a t -> 'a
    val push_back : 'a t -> 'a -> unit

    val empty : 'a t -> bool
    val toList : 'a t -> 'a list
    val ofList : 'a list -> 'a init_t -> 'a t
    val clear : 'a t -> unit
    val first : 'a t -> 'a
    val last : 'a t -> 'a
    val for_all : ('a -> bool) -> 'a t -> bool
    val exists : ('a -> bool) -> 'a t -> bool
    val app : ('a -> unit) -> 'a t -> unit
  end

module ListDeq : (FUNDEQ with type 'a init_t = unit) =
  struct
	type 'a t = 'a list * 'a list
	type 'a init_t = unit
	let mk () = ([],[])

	let unshift v (h_lr,t_rl) = (v::h_lr,t_rl)
	let push_front = unshift
	let push (h_lr,t_rl) v = (h_lr, v::t_rl)
	let push_back = push

	let rec front = function
		(h::h_lr,rl) as t -> Some h, t
	  | ([],[]) as t -> None, t
	  | ([],rl) -> front (List.rev rl,[])
	let rec shift = function
		(h::h_lr,rl) -> h,(h_lr,rl)
	  | ([],[]) -> raise Not_found
	  | ([],rl) -> shift (List.rev rl,[])
	let pop_front = shift

	let rec back = function
	(lr,h::t_rl) as t -> t, Some h
	  | ([],[]) as t -> t, None
	  | (lr,[]) -> back ([], List.rev lr)
	let rec pop = function
		(lr,h::t_rl) -> (lr,t_rl),h
	  | ([],[]) -> raise Not_found
	  | (lr,[]) -> pop ([], List.rev lr)
	let pop_back = pop
     

	let empty = function
		([],[]) -> true
	  | _ -> false

	let clear _ = ([],[])

	let toList (lr,rl) = lr@(List.rev rl)
	let ofList l () = (l,[])
	let first d = fst(shift d)
	let last d = snd(pop d)

	let for_all pred (lr,rl) = List.for_all pred lr && List.for_all pred rl
	let exists pred (lr,rl) = List.exists pred lr || List.exists pred rl

	let app f (lr,rl) =
	  List.iter f lr;
	  List.iter f (List.rev rl)
  end

module MkMutDeq(FD: FUNDEQ) : (MUTDEQ with type 'a init_t = 'a FD.init_t) =
  struct
	type 'a init_t = 'a FD.init_t
	type 'a t = ('a FD.t ref * 'a init_t)
	let mk v = (ref(FD.mk v),v)

	let shift (d,_) =
	  let (v,d') = FD.shift !d
	  in d:=d'; v
	let pop_front = shift
					  
	let pop (d,_) =
	  let (d',v) = FD.pop !d
	  in d:=d'; v
	let pop_back = pop

	let front (d, _) =
	  let (v, d') = FD.front !d in
	  d := d' ;
	  v

	let back (d, _) =
	  let (d', v) = FD.back !d in
	  d := d' ;
	  v

	let unshift v (d,_) =  d := FD.unshift v !d
	let push_front = unshift
	let push (d,_) v =  d := FD.push !d v
	let push_back = push

	let empty (d,_) = FD.empty !d
	let clear (d,init) = d := FD.mk init

	let toList (d,_) = FD.toList !d
	let ofList l init = (ref(FD.ofList l init),init)

	let clear (d,init) = d := FD.mk init
	let first (d,_) = FD.first !d
	let last (d,_) = FD.last !d

	let for_all pred (d,_) = FD.for_all pred !d
	let exists pred (d,_) = FD.exists pred !d
	let app f (d,_) = FD.app f !d
  end

module MutListDeq = MkMutDeq(ListDeq)

module BoundedArrayMap :(ORDERED_FOMUTMAP with type dom = int and type 'rng init_t = int) =
struct
  type 'rng init_t = int
  type dom = int
  type 'rng t =
      {
	mutable count : int ;
	v : 'rng option array ;
      }

  let mk size =
    {
      count = 0 ;
      v = Array.create size None ;
    }

  let map m k =
    match m.v.(k) with
	None -> raise Not_found
      | Some v -> v

  let size m = m.count
  let empty m = 0 = size m
  let clear m =
    for i = 0 to (Array.length m.v) - 1 do
      m.v.(i) <- None
    done ;
    m.count <- 0

  let for_all pred m =
    let v = m.v in
    let alen = Array.length v in
    let rec forec i =
      if i = alen then true
      else
	match v.(i) with
	    None -> forec (i+1)
	  | Some v ->
	      if not (pred i v) then false
	      else forec (i+1)
    in forec 0

  let exists pred m =
    let v = m.v in
    let alen = Array.length v in
    let rec erec i =
      if i = alen then false
      else
	match v.(i) with
	    None -> erec (i+1)
	  | Some v ->
	      if pred i v then true
	      else erec (i+1)
    in erec 0

  let min m =
    if empty m then failwith "BoundedArraymap.min" ;
    let v = m.v in
    let alen = Array.length v in
    let rec minrec i =
      if i = alen then assert false
      else
	match v.(i) with
	    None -> minrec (i+1)
	  | Some _ -> i
    in minrec 0

  let max m =
    if empty m then failwith "BoundedArraymap.max" ;
    let v = m.v in
    let alen = Array.length v in
    let rec maxrec i =
      if i = 0 then assert false
      else
	match v.(i-1) with
	    None -> maxrec (i-1)
	  | Some _ -> i-1
    in maxrec alen

  let app f m =
    Array.iteri
	(fun i ->
	   function
	       None -> ()
	     | Some v -> f i v) m.v

  let fold f initv m =
    let rv = ref initv in
      app (fun k v -> rv := f initv (k,v)) m ;
      !rv

  let try_find f m =
    let rv = ref None in
      app (fun k v -> try rv := Some (f (k,v)) with Failure _ -> ()) m ;
      match !rv with
	  None -> failwith "BoundedArrayMap.try_find"
	| Some v -> v

  let toList m =
    let acc = ref [] in
      app (fun i v -> push acc (i,v)) m ;
      List.rev !acc

  let dom m = List.map fst (toList m)
  let rng m = List.map snd (toList m)

  let remap m k v' =
    assert (0 <= k) ;
    assert (k <= Array.length m.v) ;
    match m.v.(k) with
	None -> invalid_arg "BoundedArrayMap.remap"
      | Some _ -> m.v.(k) <- Some v'

  let rmv m k =
    assert (0 <= k) ;
    assert (k <= Array.length m.v) ;
    match m.v.(k) with
	None -> invalid_arg "BoundedArrayMap.rmv"
      | Some _ ->
	  assert (m.count > 0) ;
	  m.v.(k) <- None ;
	  m.count <- m.count - 1

  let add m (k,v) =
    assert (0 <= k) ;
    assert (k <= Array.length m.v) ;
    match m.v.(k) with
	Some _ -> invalid_arg "BoundedArrayMap.add"
      | None ->
	  m.count <- m.count + 1 ;
	  m.v.(k) <- Some v

  let inv m v =
    let acc = ref [] in
      app (fun i v' -> if v=v' then push acc i) m ;
      List.rev !acc

  let in_rng m v =
    exists (fun i v' -> v=v') m

  let in_dom m k =
    exists (fun i v -> i = k) m

end

module ArrayMap : (ORDERED_FOMUTMAP with type dom = int) =
struct
  module BAM = BoundedArrayMap

  type 'rng init_t = int
  type dom = int

  type 'rng t =
      {
	chunksize : int ;
	chunks : (int, 'rng BAM.t) MOLM.t ;
      }

  let mk chunksize =
    {
      chunksize = chunksize ;
      chunks = MOLM.mk () ;
    }

  let min m =
    if MOLM.empty m.chunks then failwith "ArrayMap.min" ;
    let bam = MOLM.map m.chunks (MOLM.min m.chunks) in
      BAM.min bam

  let max m =
    if MOLM.empty m.chunks then failwith "ArrayMap.max" ;
    let bam = MOLM.map m.chunks (MOLM.max m.chunks) in
      BAM.max bam

  let _quantize m k =
    let rem = k mod m.chunksize in
    let rem = if rem < 0 then 4+rem else rem in
      k-rem

  let size m =
    let acc = ref 0 in
      MOLM.app (fun _ bam -> acc := BAM.size bam + !acc) m.chunks ;
      !acc

  let exists pred m =
    MOLM.exists (fun qk bam -> BAM.exists (fun i v -> pred (qk+i) v) bam) m.chunks

  let for_all pred m =
    MOLM.for_all (fun qk bam -> BAM.for_all (fun i v -> pred (qk+i) v) bam) m.chunks

  let app f m =
    MOLM.app (fun qk bam -> BAM.app (fun i v -> f (qk+i) v) bam) m.chunks

  let fold f initv m =
    MOLM.fold (fun c (qk,bam) -> BAM.fold (fun c (i,v) -> f c (qk+i,v)) c bam) initv m.chunks

  let try_find f m =
    MOLM.try_find (fun (qk,bam) -> BAM.try_find (fun (i,v) -> f (qk+i,v)) bam) m.chunks

  let toList m =
    let acc = ref [] in
      app (fun i v -> push acc (i,v)) m ;
      List.rev !acc

  let dom m = List.map fst (toList m)
  let rng m = List.map snd (toList m)

  let in_rng m v =
    MOLM.exists (fun qk bam -> BAM.in_rng bam v) m.chunks

  let in_dom m k =
    let qk = _quantize m k in
      (MOLM.in_dom m.chunks qk) &&
	(let bam = MOLM.map m.chunks qk in
	   BAM.in_dom bam (k -qk))

  let map m k =
    let qk = _quantize m k in
      if not(MOLM.in_dom m.chunks qk) then raise Not_found
      else BAM.map (MOLM.map m.chunks qk) (k-qk)

  let empty m = MOLM.empty m.chunks
  let clear m = MOLM.clear m.chunks

  let inv m v =
    let acc = ref [] in
      app (fun k v' -> if v=v' then push acc k) m ;
      List.rev !acc

  let remap m k v =
    try
      let qk = _quantize m k in
      let bam = MOLM.map m.chunks qk in
	BAM.remap bam (k-qk) v
    with (Not_found|Invalid_argument _) as e -> invalid_arg "ArrayMay.remap"

  let rmv m k =
    try
      let qk = _quantize m k in
      let bam = MOLM.map m.chunks qk in
	if BAM.in_dom bam (k-qk) then (
	  BAM.rmv bam (k-qk) ;
	  if BAM.empty bam then
	    MOLM.rmv m.chunks qk
	)
	else
	  invalid_arg "ArrayMap.rmv"
    with Not_found as e -> invalid_arg "ArrayMap.rmv"

  let add m (k,v) =
    let qk = _quantize m k in
      if not(MOLM.in_dom m.chunks qk) then
	MOLM.add m.chunks (qk,BAM.mk m.chunksize) ;
    let bam = MOLM.map m.chunks qk in
      if BAM.in_dom bam (k-qk) then
	invalid_arg "ArrayMap.add"
      else
	BAM.add bam (k-qk, v)

end
