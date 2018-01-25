(* problem 7*)
let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst ->
	let rec test f l = 
		match l with
		|[] -> []
		|hd::tl -> f hd :: test f tl in 
	let l1 = test (fun (x,_) -> x) lst in
	let l2 = test (fun (_,x) -> x) lst in
	(l1,l2);;