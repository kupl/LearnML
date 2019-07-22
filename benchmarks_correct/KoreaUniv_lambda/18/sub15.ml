type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string


let rec extend_env x env = x::env;;

let rec is_in lam env = match lam with
  | V x -> (match env with
    |[] -> false
    |hd::tl -> if x=hd then true else is_in lam tl)
  | P (x, lam1) -> is_in lam1 (x::env)
  | C (lam1, lam2) -> 
    (is_in lam1 env) && (is_in lam2 env);;

let check : lambda -> bool
= fun lam -> is_in lam [];;

let pgm1 = P ("a", V "a");;
let pgm2 = P ("a", P ("a", V "a"));;
let pgm3 = P ("a", P ("b", C (V "a", V "b")));;
let pgm4 = P ("a", C (V "a", P ("b", V "a")));;
let pgm5 = P ("a", V "b");;
let pgm6 = P ("a", C (V "a", P ("b", V "c")));;
let pgm7 = P ("a", P ("b", C (V "a", V "c")));;
check pgm1;;
check pgm2;;
check pgm3;;
check pgm4;;
check pgm5;;
check pgm6;;
check pgm7;;