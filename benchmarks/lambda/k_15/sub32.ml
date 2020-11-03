  type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
  and var = string
  
  let rec contain
	= fun str lst -> match lst with
		[] -> false
		|h::t -> if (h = str) then true else contain str t

  let rec chk
	= fun lambdar env -> match lambdar with
		V str -> contain str env
		|P (s, e) -> chk e (env@[s])
		|C (e1, e2) -> chk e1 env && chk e2 env
		
  let check : lambda -> bool
  =fun e -> chk e []
