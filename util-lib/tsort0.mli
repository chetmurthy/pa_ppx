type 'a edges_t

type 'a visit_type = PRE of 'a | POST of 'a | EDGE of 'a * 'a

val dfs :
  ('a -> 'b -> 'a) ->
  ('a -> 'b visit_type -> unit) ->
  'b list -> ('b, 'b edges_t) Hashtbl.t -> 'a -> 'b list option -> unit

val mkadj : ('a * 'a) list -> ('a, 'a edges_t) Hashtbl.t

val cycles :
  'a list -> ('a, 'a edges_t) Hashtbl.t -> 'a Uf.t * ('a * 'a list) list

val cyclic : 'a list -> ('a, 'a edges_t) Hashtbl.t -> bool

val tsort : 'a list -> ('a, 'a edges_t) Hashtbl.t -> 'a list list

val tclos : 'a list -> ('a, 'a edges_t) Hashtbl.t -> 'a list -> 'a list
