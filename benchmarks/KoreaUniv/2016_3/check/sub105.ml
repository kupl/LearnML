
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let check : exp -> bool
  = fun exp -> true;;

	let rec check2
	= fun (exp,lis) ->
	match  exp with
	|C (exp1,exp2) -> if(check2(exp1,lis)&&check2(exp2,lis)) then true else false
	|V vars -> (match lis with
			|hd::tl -> if (vars=hd) then true else check2(V vars, tl)
			|[] -> false)
	|P (vars, exp2) -> check2(exp2, vars::lis);;
	

	let check : exp -> bool
	= fun exp -> check2 (exp,[]);; 
