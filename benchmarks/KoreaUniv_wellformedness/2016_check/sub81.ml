
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

let rec isin (str,expr) = match (str,expr) with
			|(str,V a) -> false
			|(str, P(a,b)) -> if a= str then true else isin(str,b)
			|(str, C(a,b)) ->false

let rec findvar exp expression = match exp with	
					|V var -> isin(var, expression)
					|P (var,expr) -> ((findvar expr expression) || (findvar expr exp))
					|C (expr1,expr2) -> ((findvar expr1 expression) ||( findvar expr1 expr1)) && ((findvar expr2 expression)||(findvar expr2 expr2))

  let rec check : exp -> bool
  = fun exp -> match exp with
				|V var ->false
				|P (var,expr) -> (findvar expr exp)
				|C(expr1,expr2) -> (check expr1) &&(check expr2)
