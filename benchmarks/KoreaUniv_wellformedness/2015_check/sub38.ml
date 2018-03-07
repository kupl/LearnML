  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
  
  let rec check : exp -> bool
  =fun e -> match e with
|P(v,e)-> (match e with
		|V a -> v=a
		|P(a,b) -> check e
		|C(a,b) -> (check a) && (check b)
	  )
|C(a,b)-> (check a) && (check b)
|V a -> true
|_ -> false
