
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

	let rec para : (lambda*string list) -> bool
	= fun (lambda, lst) -> match lambda with
	| V(x) -> (let rec ac : (string list*string) -> bool
						=fun (oh, ho) -> (match oh with
						|[]-> false
						|hd::tl-> if(hd=ho) then true else (ac(tl, ho))
						)
						in ac(lst, x))   
	| P(x,e) -> para(e, x::lst)
	| C(e1, e2) -> ((para(e1, lst))&&(para(e2, lst)))


  let rec check : lambda -> bool
  = fun lambda -> para(lambda, [])
