  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string

	let rec inlist : string * string list -> bool (* Is it in list? *)
	=fun (a,l) -> match l with
								|[]-> false
								|hd::tl -> if hd = a then true else inlist(a,tl)



	let rec chlist : exp * string list -> bool (* Check with list*)
	=fun (e,l) -> match e with
							|V a -> inlist(a,l)
							|P(a,env) -> chlist(env,a::l)
							|C(env1,env2) -> chlist(env1,l) && chlist(env2,l)

  let rec check : exp -> bool
  =fun e -> chlist(e,[])
