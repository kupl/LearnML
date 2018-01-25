(* problem 7*)

let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> (*TODO*)
	let rec first lst =
		match lst with
		|[] -> []
		|(a,b)::t -> a::(first t) in
			let rec last lst =
				match lst with
				|[] -> []
				|(a,b)::t -> b::(last t) in
					(first lst, last lst)
