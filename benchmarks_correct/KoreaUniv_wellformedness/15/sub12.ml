  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
  let rec have v env = 
      match env with
      | [] -> false
      | hd::tl -> if hd = v then true else have v tl
    
	let extend x env = x::env
  
  let rec eval : exp -> var list -> bool
  = fun e env -> match e with
    | V a -> have a env
    | P (a, b) -> eval b (extend a env)
    | C (a, b) -> if (eval a env) && (eval b env) then true else false
  
  let check : exp -> bool
  =fun e -> eval e []
