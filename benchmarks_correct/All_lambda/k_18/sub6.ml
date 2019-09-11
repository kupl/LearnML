type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let rec find_stack
= fun v stack -> match stack with
  | hd::tl -> if hd=v then true else find_stack v tl 
  | [] -> false;;
  
let rec check_inner
= fun lam stack -> match lam with
  | V v -> if find_stack v stack then true else false
  | P (v, l) -> check_inner l (v::stack)
  | C (l1, l2) -> (check_inner l1 stack) && (check_inner l2 stack);;

let check : lambda -> bool
= fun lam -> check_inner lam [];;
