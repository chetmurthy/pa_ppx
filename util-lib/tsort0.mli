type 'a edges_t
type 'a hash_adj_t

type 'a visit_type = PRE of 'a | POST of 'a | EDGE of 'a * 'a

val dfs :
  ('a -> 'b -> 'a) ->
  ('a -> 'b visit_type -> unit) ->
  'b list -> 'b hash_adj_t -> 'a -> 'b list option -> unit

val mkadj : ('a * 'a) list -> 'a hash_adj_t

val cycles :
  'a list -> 'a hash_adj_t -> 'a Uf.t * ('a * 'a list) list

val cyclic : 'a list -> 'a hash_adj_t -> bool

val tsort : 'a list -> 'a hash_adj_t -> 'a list list

val tclos : 'a list -> 'a hash_adj_t -> 'a list -> 'a list
