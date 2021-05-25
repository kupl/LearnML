type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let rec find_var : var -> var list -> var list
= fun x lst ->
  match lst with
  | [] -> []
  | h::t -> if h = x then (find_var x t) else h::(find_var x t);;

let rec eval : lambda -> var list
= fun lam ->
  match lam with
  | V x -> [x]
  | P (x, l) -> find_var x (eval l)
  | C (l1, l2) -> (eval l1) @ (eval l2);;

let check : lambda -> bool
= fun lam ->
  if (eval lam) = [] then true else false;;
  
