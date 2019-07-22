  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
  
  let rec contain
	= fun str lst -> match lst with
		[] -> false
		|h::t -> if (h = str) then true else contain str t

  let rec chk
	= fun expr env -> match expr with
		V str -> contain str env
		|P (s, e) -> chk e (env@[s])
		|C (e1, e2) -> chk e1 env && chk e2 env
		
  let check : exp -> bool
  =fun e -> chk e []
