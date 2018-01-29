
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let rec check_lst lst elem =
	match lst with
	| hd::tl -> if hd = elem then true else check_lst tl elem
	| [] -> false
 
  let check : exp -> bool
  = fun exp ->
	let rec check_helper exp bound_list =
	match exp with
	| P (e1, e2) -> 
		let lis = e1::bound_list in
		check_helper e2 lis
	| V v -> check_lst bound_list v
	| C (e1, e2) -> (check_helper e1 bound_list) && (check_helper e2 bound_list)
	in
	let bound_list = [] in
	check_helper exp bound_list
