type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let env = ref [];;

let rec check : lambda -> bool
= fun lam -> 
  match lam with 
    |V v -> 
      let rec findv lst =
      match lst with
        |[] -> false
        |hd::tl -> if hd = v then true else findv tl in
        findv !env
    |P(v,l) -> env := v::!env; check l
    |C(l1,l2) -> (check l1) && (check l2);;
  
        
    
    