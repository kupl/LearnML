let rec zipper : int list * int list -> int list
=fun (a,b) -> 
	match b with
		[]->a
		|hd::tl->(match a with
				[]->b
				|h::t->h::hd::zipper (t,tl));;
