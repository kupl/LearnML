type btree =
	| Empty
	| Node of int * btree * btree

let rec f : int -> btree -> bool
= fun n tree -> match tree with
| Empty -> false
| Node(nt, l, r) ->
if nt = n then true
else (f n l) && (f n r);;
