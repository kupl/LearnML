  type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
  and var = string

let rec checklist: var list->var->bool
=fun lst x->match lst with
	|[]->false
	|hd::tl->if x==hd then true else (checklist tl x)
			
		
let rec makelist: var list->lambda->bool
=fun lst e->match e with
	V x->checklist lst x
	|P(a,e)->makelist (lst@[a]) e
	|C(e1,e2)->(makelist lst e1)&&(makelist lst e2)
		
  
let rec check : lambda -> bool
  =fun e ->makelist [] e   


