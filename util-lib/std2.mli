
val hash_uniq : 'a list -> 'a list          (* remove redundant elements *)
val hash_list_repeats : 'a list -> 'a list  (* return only redundant elements *)
val hash_union : 'a list -> 'a list -> 'a list (* union that respects order of elements in args *)
    
