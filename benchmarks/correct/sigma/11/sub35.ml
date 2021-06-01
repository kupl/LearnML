(* 2009-13384, CHO Hyunik *)


let rec sigma f a b =
	match a>b with
	true -> 0
	| _ -> f a + sigma f (a+1) b