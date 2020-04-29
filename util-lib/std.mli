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
(*                                 std.mli                                  *)
(****************************************************************************)

val pair : 'a -> 'b -> 'a * 'b

val fst3 : ('a * 'b * 'c) -> 'a
val snd3 : ('a * 'b * 'c) -> 'b
val third3 : ('a * 'b * 'c) -> 'c

val fst4 : ('a * 'b * 'c * 'd) -> 'a
val snd4 : ('a * 'b * 'c * 'd) -> 'b
val third4 : ('a * 'b * 'c * 'd) -> 'c
val fourth4 : ('a * 'b * 'c * 'd) -> 'd

val nth1 : 'a list -> int -> 'a
	(* returns the n-th element of the given list, where the head of the list is at position 1 *)

val sep_firstn : int -> 'a list -> 'a list * 'a list
val sep_last : 'a list -> 'a * 'a list
val sizebin_of_string : string -> int
val sizebin64_of_string : string -> int64
val string_of_sizebin64 : int64 -> string
val string_of_sizebin : int -> string

val car : 'a list -> 'a
val cdr : 'a list -> 'a list
val cddr : 'a list -> 'a list
val cdddr : 'a list -> 'a list
val safe_cdr : 'a list -> 'a list
val safe_cddr : 'a list -> 'a list
val safe_cdddr : 'a list -> 'a list
val cadr : 'a list -> 'a
val caddr : 'a list -> 'a
val except : 'a -> 'a list -> 'a list
val try_find : ('a -> 'b) -> 'a list -> 'b
val filter : ('a -> bool) -> 'a list -> 'a list
val last : 'a list -> 'a
val firstn : int -> 'a list -> 'a list

(* Maps f on the sublist of l where f does not raise an exception Failure _ *)
(* For example, if (f x2) and (f x4) both raise the exception Failure _
    map_succed f [x1; x2; x3; x4; x5; x6] 
  evaluates to [f x1; f X3; f x5; f x6] *)
val map_succeed : ('a -> 'b) -> 'a list -> 'b list
val map3 : ('a -> 'b -> 'c -> 'd) -> 'a list -> 'b list -> 'c list -> 'd list

val distinct : 'a list -> bool

val interval : int -> int -> int list
val interval_int32 : int32 -> int32 -> int32 list
val range : int -> int list
val range_int32 : int32 -> int32 list

val push : 'a list ref -> 'a -> unit
val pop : 'a list ref -> unit
val top : 'a list ref -> 'a

type ('a,'b) union = Inl of 'a | Inr of 'b
val isl : ('a,'b) union -> bool
val isr : ('a,'b) union -> bool
val outl : ('a,'b) union -> 'a
val outr : ('a,'b) union -> 'b

(************************************************************************)
(* sets operations on lists;                                            *)
(* for an abstract and baltree version of sets, use module Set          *)

val uniquize : 'a list -> 'a list          (* remove redundant elements *)
val make_set : 'a list -> 'a list                (* synonym of uniquize *)
val add_set  : 'a -> 'a list -> 'a list           (* add if not present *)
val rmv_set  : 'a -> 'a list -> 'a list

val intersect : 'a list -> 'a list -> 'a list
val union     : 'a list -> 'a list -> 'a list
val union2    : 'a list -> 'a list -> 'a list  (* optim. for large sets *)
val unionq    : 'a list -> 'a list -> 'a list   (* with == instead of = *)
val diff_set  : 'a list -> 'a list -> 'a list
val subtract  : 'a list -> 'a list -> 'a list    (* synonym of diff_set *)
val subtractq : 'a list -> 'a list -> 'a list   (* with == instead of = *)
val symdiff   : 'a list -> 'a list -> 'a list

val subset    : 'a list -> 'a list -> bool
val same_members : 'a list -> 'a list -> bool

val map_option : ('a -> 'b) -> 'a option -> 'b option

val isNone : 'a option -> bool
val isSome : 'a option -> bool
val inSome : 'a -> 'a option
val outSome : 'a option -> 'a

val plist : ('a Stream.t -> 'b) -> 'a Stream.t -> 'b list
val plist_until : ('a Stream.t -> 'b list) -> ('a Stream.t -> 'b) -> 'a Stream.t -> 'b list

val parse_fully : ('a Stream.t -> 'b) -> 'a Stream.t -> 'b

val string_suffix : string -> int -> string

val starts_with : ?improper:bool -> pat:string -> string -> bool
val ends_with : pat:string -> string -> bool

val fold_option : ('a -> 'b) -> 'b -> 'a option -> 'b

val apply_to_in_channel : (in_channel -> 'a) -> string -> 'a

val apply_to_out_channel : (out_channel -> 'a) -> string -> 'a

val do_option : ('a -> unit) -> 'a option -> unit
val implode_chars : char list -> string
val list_of_stream : 'a Stream.t -> 'a list
val nway_partition : ('a -> 'a -> bool) -> 'a list -> 'a list list
val read_ic_fully : ?msg:string -> ?channel:in_channel -> unit -> string

val app_i : (int -> 'a -> 'b) -> int -> 'a list -> unit

module ColumnFormat :
  sig
    val format0 : int -> string list -> string array array
    val formatted_width :
      ncols:int -> string list -> int * int array * string array array
    val format : width:int -> string list -> string array array
    val output : ?width:int -> out_channel -> string list -> unit
  end

val invoked_with : ?flag:string -> string -> bool
val split : ('a * 'b) list -> ('a list * 'b list)
val split_option : ('a * 'b) option -> ('a option * 'b option)
val combine : 'a list -> 'b list -> ('a * 'b) list
val slice_string : ?spos:int -> ?epos:int -> string -> string
val pred_or : ('a -> bool) list -> 'a -> bool
