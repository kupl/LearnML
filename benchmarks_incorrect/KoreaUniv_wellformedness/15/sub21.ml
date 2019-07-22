  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string

  let rec extract : exp -> var list
  =fun e -> match e with 
  			| V v -> []
  		    | P (v1, e1) -> [v1] @ extract e1
  		    | C (e1, e2) -> extract e1 @ extract e2

  let rec search : var * var list -> bool
  =fun (v, lst) -> match lst with
  				   | [] -> false
  				   | head :: [] -> if v = head then true 
  								   else false
  				   | head :: tail -> if ((v = head) || search (v, tail)) then true 
  									 else false
 
  let rec check2 : exp * var list -> bool
  =fun (e, a) -> match e with
  			     | V v1 -> search (v1, a)
  			     | P (v1, e1) -> check2 (e1, a)
  		   	     | C (e1, e2) -> check2 (e1, a) && check2 (e2, a)

  let check : exp -> bool
  =fun e -> check2 (e, extract e)
 