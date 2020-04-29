
type 'a edges_t = {mutable edges: 'a list}
type 'a hash_adj_t = ('a, 'a edges_t) Hashtbl.t

let nodes edgl =
  let s = Hashtbl.create 23 in
  let nodes = ref [] in
  let chkadd n =
	if not(Hashtbl.mem s n) then
	  begin
		Hashtbl.add s n ();
		nodes := n :: !nodes
	  end
  in
	List.iter
	  (fun (a,b) -> chkadd a; chkadd b)
	  edgl;
	!nodes

type 'a visit_type = PRE of 'a | POST of 'a | EDGE of 'a * 'a

let dfs stf visitf nodes dadj stk stopt =
  let visited = Hashtbl.create 23 in
  let rec dfsrec st n =
	if not(Hashtbl.mem visited n) then
	  begin
		Hashtbl.add visited n ();
		let edges = (Hashtbl.find dadj n).edges in
		let st' = stf st n
		in visitf st (PRE n);
		  List.iter (fun n' -> visitf st' (EDGE(n,n')); dfsrec st' n') edges;
		  visitf st (POST n)
	  end
  in match stopt with
	  Some nl -> List.iter (dfsrec stk) nl
	| None ->
		List.iter (fun n -> if not(Hashtbl.mem visited n) then dfsrec stk n) nodes

let mkadj edgl =
  let dadj = Hashtbl.create 23 in
  let nodelist = nodes edgl in
	List.iter (fun n -> Hashtbl.add dadj n {edges=[]}) nodelist;
  let _ = List.iter
			(fun (a,b) ->
			   let edges = Hashtbl.find dadj a
			   in edges.edges <- b::edges.edges) edgl in
	dadj

let cycles nodes dadj =
  let uf = Uf.mk nodes in
  let cyfind n stk =
	let rec cyrec = function
		h::_ as l when (Uf.find uf h)=(Uf.find uf n) -> l
	  | h::t -> cyrec t
	  | [] -> []
	in cyrec (List.rev stk) in
  let () = dfs (fun stk n -> n::stk)
			 (fun stk -> function
				  (EDGE(n,n')) -> List.iter (Uf.union uf n') (cyfind n' stk)
				  | _ -> ()) nodes dadj [] None in
  let cynodes = ref [] in
  let cyh = Hashtbl.create 23 in
  let () = List.iter
			 (fun n ->
				let rep = (Uf.find uf n) in
				  if rep<>n then
					begin
					  (if not(Hashtbl.mem cyh rep) then
						 (cynodes := rep :: !cynodes));
					  Hashtbl.add cyh rep n
					end)
			 nodes in
	(uf,List.map
	   (fun n -> (n,n::(Hashtbl.find_all cyh n))) !cynodes)

let cyclic nodes dadj =
  let (_, cl) = cycles nodes dadj in
  cl <> []

let tsort nodes dadj =
  let (uf,cyclel) = cycles nodes dadj in
  let packets = ref [] in
  let pushed = Hashtbl.create 23 in
  let () = dfs (fun stk n ->
				  if not(List.exists (fun m -> (Uf.find uf m)=(Uf.find uf n)) stk) then
				  	n::stk
				  else
					stk)
			 (fun stk -> function
				  POST n ->
					let rep = (Uf.find uf n) in
					  (if not(Hashtbl.mem pushed rep) && not(List.exists (fun m -> (Uf.find uf n)=(Uf.find uf m)) stk) then
						 (packets := rep :: !packets;
					   	  Hashtbl.add pushed rep ()))
				| _ -> ()) nodes dadj [] None in
  let packl = List.rev !packets in
  let cyh = Hashtbl.create 23 in
  let _ = List.iter (fun (_,l) -> List.iter (fun n -> Hashtbl.add cyh n l) l) cyclel in
	List.map
	  (fun n ->
		 try List.assoc n cyclel
		 with Not_found -> [n]) packl

let tclos nodes dadj nl =
  let l = ref [] in
  let _ =
	dfs (fun stk n -> n::stk)
	  (fun stk -> function
		   PRE n -> l := n :: !l
		 | _ -> ())
	  nodes dadj [] (Some nl) in
	!l
