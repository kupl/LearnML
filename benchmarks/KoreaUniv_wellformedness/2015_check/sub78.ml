  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
  
  let check : exp -> bool
  = fun e -> let rec envcheck : exp * exp list -> bool
  = fun (e,l) -> begin
           match e with
            V x -> let rec varcheck : exp list -> bool = fun l2 -> begin match l2 with 
                                                                [] -> false
                                                               | h::t -> if h=V x then true else varcheck(t)
                                                             end  in varcheck(l)  (*check if variable is in the environment return true*)
          | P(v,e) -> envcheck (e,(V v)::l) 
          | C(e1,e2) -> if envcheck (e1,l)&&envcheck (e2,l) then true else false 
          end in envcheck (e,[])
