type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let rec lookup_env x e = 
  match e with
  | [] -> false
  | y::tl -> if x = y then true else lookup_env x tl

let rec checkhel : lambda -> var list -> bool
= fun lam env -> match lam with
  | V v -> lookup_env v env
  | P (v, l) -> checkhel l (v:: env)
  | C (l1, l2) -> if (((checkhel l1 env) = true)&&((checkhel l2 env) = true)) then true else false
  
  
let check : lambda -> bool
= fun lam -> checkhel lam [];; 

check( P ("a", V "a"));;
check( P ("a", P ("a", V "a")));;
check(P ("a", P ("b", C (V "a", V "b"))));;
check(P ("a", C (V "a", P ("b", V "a"))));;
check (P("a", V "b"));;
check(P ("a", C (V "a", P ("b", V "c"))));;
check(P ("a", P ("b", C (V "a", V "c"))));;