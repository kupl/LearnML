type btree = Empty | Node of int * btree * btree;;
let rec mem n t = match t with
	|Empty -> false
	|Node (p,l,r) ->
	if p==n then true
	else if mem n l then true
	else if mem n r then true
	else false;;