
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string


  let rec compare : exp -> string list -> bool
  = fun exp lst ->
    match exp with
    | V x -> 
			begin
      match lst with
      | [] -> false
      | hd::tl -> if x = hd then true
                  else compare exp tl
			end
    | P (x, exp1) -> compare exp1 (x::lst)
		| C (exp1, exp2) -> (compare exp1 lst) && (compare exp2 lst)


  let rec check : exp -> bool
  = fun exp ->
		let lst = [] in   
		match exp with
		| V x -> false
		| P (x, exp1) -> compare exp1 (x::lst)
		| C (exp1, exp2) -> (compare exp1 lst) && (compare exp2 lst)

