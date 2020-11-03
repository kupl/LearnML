
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

	let rec deletefrom : var list * var -> var list
	= fun (varlist1, vardel) -> match varlist1 with
	| [] -> []
	| hd::tl -> if hd=vardel then deletefrom(tl, vardel) else [hd]@(deletefrom(tl, vardel))

	and addfor : var list * var -> var list
	= fun (varlist1, varadd) -> match varlist1 with
	| [] -> [varadd]
	| hd::tl -> if hd=varadd then varlist1 else [hd]@(addfor(tl, varadd))

	and check2 : lambda * var list -> var list
	= fun (lambda, varlist1) -> match lambda with
	| V var1 -> addfor(varlist1, var1)
	| P (var1, lambda1) -> deletefrom(check2(lambda1, varlist1), var1)
	| C (lambda1, lambda2) -> (check2(lambda1, varlist1))@(check2(lambda2, varlist1))

  let check : lambda -> bool
	= fun lambda -> match check2(lambda, []) with
	| [] -> true
	| _ -> false

