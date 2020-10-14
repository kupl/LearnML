(* problem 3*)

let iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> 
	let idf n = n in
		let rec help n f =
			if n == 0 then idf
			else if n == 1 then f
			else f (*help (n-1) f f*) in
				help n f