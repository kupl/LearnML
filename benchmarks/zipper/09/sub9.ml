(* 2006-11782 Song Young-chan, Hw1-3 zipper *)

let rec zipper((list_a:int list),(list_b:int list)) = 
	match (list_a, list_b) with 
	 ([],_) -> list_b
	|(_,[]) -> list_a
	|(h_a::[], h_b::t_b) -> h_a::(h_b::t_b)
	|(h_a::t_a, h_b::t_b) -> h_a::(h_b::(zipper(t_a,t_b)))
