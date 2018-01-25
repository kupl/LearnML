(* problem 7*)
let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst ->
match lst with
	[] -> ([], [])
	|(a,b)::t -> let (at, bt) = unzip t in (a::at, b::bt)