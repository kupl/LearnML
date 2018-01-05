(* 2009-13384, CHO Hyunik *)


let rec sigma(a, b, f) =
	match a>b with
	true -> 0
	| _ -> f a + sigma(a+1, b, f)