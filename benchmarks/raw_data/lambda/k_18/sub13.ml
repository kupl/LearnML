type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let rec lookup_x x env = match env with
  |[] -> false
  |hd :: tl -> if (hd = x) then true else lookup_x x tl;;

let check : lambda -> bool
= fun lam -> let rec func lamd env = match lamd with
  | V x -> lookup_x x env
  | P (x, l) -> func l (x :: env) 
  | C (l1, l2) -> func l1 env && func l2 env
  in func lam [];;
  
check (P ("a", V "a"));;
check (P ("a", P ("a", V "a")));;
check (P ("a", P ("b", C (V "a", V "b"))));;
check (P ("a", C (V "a", P ("b", V "a"))));;

check (P ("a", V "b"));;
check (P ("a", C (V "a", P ("b", V "c"))));;
check (P ("a", P ("b", C (V "a", V "c"))));;