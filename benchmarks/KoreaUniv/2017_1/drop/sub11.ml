(* problem 6*)

let drop : 'a list -> int -> 'a list
= fun l n -> 
	let rec help l n =
		if List.length l <= n then []
		else if n == 0 then l
		else (help (match l with 
					|[] -> []
					|h::t -> t) 
						(n-1)) in
			help l n