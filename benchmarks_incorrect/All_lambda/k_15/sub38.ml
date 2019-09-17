  type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
  and var = string
  
  let rec check : lambda -> bool
  =fun e -> match e with
|P(v,e)-> (match e with
		|V a -> v=a
		|P(a,b) -> check e
		|C(a,b) -> (check a) && (check b)
	  )
|C(a,b)-> (check a) && (check b)
|V a -> true
|_ -> false
