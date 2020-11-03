type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let rec bound_check
= fun v blist ->
  match blist with
    | [] -> false
    | h::t -> if h = v then true else bound_check v t

let check : lambda -> bool
= fun lam ->
  let rec check_c
  = fun lam blist ->
    match lam with
      | V v -> bound_check v blist
      | P (v, l) -> check_c l (v::blist)
      | C (a, b) -> if check_c a blist then check_c b blist else false
  in check_c lam [];;
  

(*let test1 = P ("a", V "a");;
let test2 = P ("a", P ("a", V "a"));;
let test3 = P ("a", P ("b", C (V "a", V "b")));;
let test4 = P ("a", C (V "a", P ("b", V "a")));;

let test5 = P ("a", V "b");;
let test6 = P ("a", C (V "a", P ("b", V "c")));;
let test7 = P ("a", P ("b", C (V "a", V "c")));;*)