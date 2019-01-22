  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string

let rec checklist: var list->var->bool
=fun lst x->match lst with
	|[]->false
	|hd::tl->if x==hd then true else (checklist tl x)
			
		
let rec makelist: var list->exp->bool
=fun lst e->match e with
	V x->checklist lst x
	|P(a,e)->makelist (lst@[a]) e
	|C(e1,e2)->(makelist lst e1)&&(makelist lst e2)
		
  
let rec check : exp -> bool
  =fun e ->makelist [] e   


