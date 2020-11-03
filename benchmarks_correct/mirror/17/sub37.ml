(* -------------------------------------------- *)
(* -----------------2012210109----------------- *)
(* --------------------김진용-------------------- *)


(* ------------------problem1------------------ *)
type btree = Empty | Node of int * btree * btree
let rec mirror : btree -> btree
= fun t -> 
	match t with
	| Empty -> Empty
	| Node(d,l,r) -> Node(d, mirror r, mirror l);;
(* -------------------------------------------- *)
