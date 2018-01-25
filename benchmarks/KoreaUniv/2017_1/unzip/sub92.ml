(* problem 7-solve*)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> match lst with
		|[]->([],[])
		|(a,b)::tl-> 
		let (tla,tlb) = unzip tl in
		((a::(tla)),(b::(tlb)))