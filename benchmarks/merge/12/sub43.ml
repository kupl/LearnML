let rec merge (a, b) =
	match ( a, b ) with
	( [], [] ) -> []
	| ( _, [] ) -> a
	| ( [], _ ) -> b
	| ( ha::la, hb::lb ) ->
		if ha < hb
			then hb::( merge (a, lb) )
			else ha::( merge (la, b) )
(*
let t1 = [ 7; 2; 1 ]
let t2 = [ 9; 8; 4 ]

let rec print a =
	match a with
	[] -> print_newline()
	| hd::tl -> ( print_int hd ); ( print_string " " ); ( print tl )

let _ = print (merge (t1, t2))
*)
