let rec zipper : int list * int list -> int list
=fun (a,b) ->
	match a with
	| [] -> (match b with
		| [] -> [];
		| hb::tb -> hb::zipper (a, tb))
	| ha::ta -> (match b with
		| [] -> ha::zipper (ta, b)
		| hb::tb -> ha::hb::zipper(ta, tb));;