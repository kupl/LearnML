(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = 
	match lst with
	|[]->[]
	|(a::b) ->
		if pred a then a :: filter pred b
		else filter pred b
