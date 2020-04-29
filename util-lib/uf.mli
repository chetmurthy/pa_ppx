type ufrep
type 'a t
val mk : 'a list -> 'a t
val findaux : 'a t -> int -> int
val a2n : 'a t -> 'a -> int
val n2a : 'a t -> int -> 'a
val find : 'a t -> 'a -> 'a
val union : 'a t -> 'a -> 'a -> unit
