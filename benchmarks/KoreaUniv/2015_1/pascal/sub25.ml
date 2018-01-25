(* Problem 1 *) 
let rec pascal : int * int -> int
=fun (x,y) -> match (x,y) with
							| (0, 0) -> 1
							| (a, 0) -> 1
							| (a, b) -> if a = b then 1
													else pascal((a-1), (b-1)) + pascal((a-1), b)
