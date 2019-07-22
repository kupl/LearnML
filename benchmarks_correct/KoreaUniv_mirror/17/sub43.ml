(* problem 1*)
type btree = Empty | Node of int * btree * btree

let mirror : btree -> btree
= fun t -> (* TODO *)
	let rec sub u =
		match u with
		| Empty -> Empty
		| Node (d,l,r) -> Node(d,sub r,sub l)
	in sub t
