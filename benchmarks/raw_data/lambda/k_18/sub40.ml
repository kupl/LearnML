type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let rec is_in_list a lst =
  match lst with
    [] -> false
    |x::xs -> if x = a then true else is_in_list a xs

let check : lambda -> bool
= fun lam ->
  let rec checkocc e varlist =
    match e with
      V var -> if is_in_list var varlist then true else false
      |P (var, lamb) -> checkocc lamb (var::varlist)
      |C (lamb1, lamb2) -> checkocc lamb1 varlist && checkocc lamb2 varlist
  in checkocc lam [];;

check (P ("a", C (V "a", P ("b", V "a"))));;
check (P ("a", P ("b", C (V "a", V "c"))));;
check (P ("a", C (V "a", P ("b", V "c"))));;
