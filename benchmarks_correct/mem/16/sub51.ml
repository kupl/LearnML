type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree ->
  match tree with
  | Empty -> false
  | Node (x, lchild, rchild) -> (x = n) || (mem n lchild) || (mem n rchild)
