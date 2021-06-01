
  	type lambda =
  	| V of var
  	| P of var * lambda
  	| C of lambda * lambda
  	and var = string

let rec compare : lambda -> string list -> bool
  	= fun lambda lst ->
    match lambda with
    | V x -> begin
    	match lst with
      	| hd::tl -> if x = hd then true else compare lambda tl
      	| [] -> false
		end
    | P (x, e1) -> compare e1 (x :: lst)
	| C (e1, e2) -> (compare e1 lst) && (compare e2 lst)

let rec check : lambda -> bool
 	= fun lambda ->
		let lst = [] in   
		match lambda with
		| V x -> false
		| P (x, e1) -> 
			let l = (x :: lst) in compare e1 l
		| C (e1, e2) -> (compare e1 lst) && (compare e2 lst)