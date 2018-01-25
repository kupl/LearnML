let rec dfact : int -> int = fun n ->
	match n with
	1 -> 1
 |2 -> 2
 |_ -> n*(dfact (n-2));;