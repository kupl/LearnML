type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam ->
let rec fchk : var -> (var list) -> (var list)
  = fun v l ->
   match l with
   | [] -> []
   | hd::tl -> 
     if v = hd then fchk v tl else hd::(fchk v tl)
  in let rec eval : lambda -> (var list)
    = fun lam ->
      match lam with
      | V x -> [x]
      | P(x,e) -> fchk x (eval e)
      | C(e1,e2) -> (eval e1)@(eval e2)
    in match eval lam with
      | [] -> true
      | _ -> false