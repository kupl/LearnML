  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
  
 let rec ch : exp*var -> bool 
=fun(e,a) -> match e with
V b -> if a=b then true else false
|P(b,e) -> (match e with 
						V c -> if (c=b||c=a) then true else false
						|P(c,e) -> ch(e,c)||ch(e,b)||ch(e,a)
						|C(e1,e2) -> if(a=b) then ch(e1,a)&&ch(e2,a)
						else (ch(e1,a)||ch(e1,b))&&(ch(e2,a)||ch(e2,b)))
|C(e1,e2) -> ch(e1,a)&&ch(e2,a)

  let check : exp -> bool
  =fun e ->match e with 
	V a-> false
	|P(a,e) -> ch(e,a)
	|C(e1,e2) ->false
	