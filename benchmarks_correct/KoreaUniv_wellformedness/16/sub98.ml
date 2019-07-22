
  	type exp =
  	| V of var
  	| P of var * exp
  	| C of exp * exp
  	and var = string

let rec compare : exp -> string list -> bool
  	= fun exp lst ->
    match exp with
    | V x -> begin
    	match lst with
      	| hd::tl -> if x = hd then true else compare exp tl
      	| [] -> false
		end
    | P (x, e1) -> compare e1 (x :: lst)
	| C (e1, e2) -> (compare e1 lst) && (compare e2 lst)

let rec check : exp -> bool
 	= fun exp ->
		let lst = [] in   
		match exp with
		| V x -> false
		| P (x, e1) -> 
			let l = (x :: lst) in compare e1 l
		| C (e1, e2) -> (compare e1 lst) && (compare e2 lst)