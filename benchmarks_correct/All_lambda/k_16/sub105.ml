
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

  let check : lambda -> bool
  = fun lambda -> true;;

	let rec check2
	= fun (lambda,lis) ->
	match  lambda with
	|C (lambda1,lambda2) -> if(check2(lambda1,lis)&&check2(lambda2,lis)) then true else false
	|V vars -> (match lis with
			|hd::tl -> if (vars=hd) then true else check2(V vars, tl)
			|[] -> false)
	|P (vars, lambda2) -> check2(lambda2, vars::lis);;
	

	let check : lambda -> bool
	= fun lambda -> check2 (lambda,[]);; 
