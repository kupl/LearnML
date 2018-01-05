let rec merge : int list * int list -> int list = 
	fun (l1, l2) -> match l1 with
		| [] -> l2
		| h::t -> match l2 with
			| [] -> l1
			| h1::t1 -> if h>h1 then h::merge(t,l2)
				    else h1::merge(l1,t1)

