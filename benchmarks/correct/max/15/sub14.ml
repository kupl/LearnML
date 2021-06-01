(* Problem 3 *)
let rec max l =
	match l with
	|[n]->n+0
	|h1::t1 -> match t1 with |h2::t2->if h2>h1 then max ([h2]@t2) else max ([h1]@t2)
