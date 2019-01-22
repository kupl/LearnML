
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

	let rec para : (exp*string list) -> bool
	= fun (exp, lst) -> match exp with
	| V(x) -> (let rec ac : (string list*string) -> bool
						=fun (oh, ho) -> (match oh with
						|[]-> false
						|hd::tl-> if(hd=ho) then true else (ac(tl, ho))
						)
						in ac(lst, x))   
	| P(x,e) -> para(e, x::lst)
	| C(e1, e2) -> ((para(e1, lst))&&(para(e2, lst)))


  let rec check : exp -> bool
  = fun exp -> para(exp, [])
