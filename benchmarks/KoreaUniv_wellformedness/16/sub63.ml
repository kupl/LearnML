
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

	let rec deletefrom : var list * var -> var list
	= fun (varlist1, vardel) -> match varlist1 with
	| [] -> []
	| hd::tl -> if hd=vardel then deletefrom(tl, vardel) else [hd]@(deletefrom(tl, vardel))

	and addfor : var list * var -> var list
	= fun (varlist1, varadd) -> match varlist1 with
	| [] -> [varadd]
	| hd::tl -> if hd=varadd then varlist1 else [hd]@(addfor(tl, varadd))

	and check2 : exp * var list -> var list
	= fun (exp, varlist1) -> match exp with
	| V var1 -> addfor(varlist1, var1)
	| P (var1, exp1) -> deletefrom(check2(exp1, varlist1), var1)
	| C (exp1, exp2) -> (check2(exp1, varlist1))@(check2(exp2, varlist1))

  let check : exp -> bool
	= fun exp -> match check2(exp, []) with
	| [] -> true
	| _ -> false

