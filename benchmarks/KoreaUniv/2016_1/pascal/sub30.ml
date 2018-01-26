let rec pascal : int * int -> int
= fun (n1, n2) -> 
	match n2 = 0 || n1 = n2 with
	| true -> 1
	| false -> pascal(n1-1, n2-1) + pascal(n1-1, n2)
