let rec zipper : int list * int list -> int list
=fun (a,b) ->
	match a with
	| [] -> b
	| a_hd::a_tl -> 
		(match b with
		 | [] -> a
		 | b_hd::b_tl -> a_hd::b_hd::(zipper(a_tl, b_tl))
		)
;;