type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let check : lambda -> bool
= fun lam -> 
    let rec check_in exp env =
      match exp with
      | V x -> env x
      | P (x, e) -> check_in e (fun y -> if y = x then true else env y)
      | C (l, r) -> (check_in l env) && (check_in r env)
    in
    check_in lam (fun y -> false)


let pgm1 = P ("a", V "a") ;;
let pgm2 = P ("a", P ("a", V "a"));;
let pgm3 =P ("a", P ("b", C (V "a", V "b")));;
let pgm4 = P ("a", C (V "a", P ("b", V "a")));;
let pgm5 = P ("a", V "b");;
let pgm6 = P ("a", C (V "a", P ("b", V "c")));;
let pgm7 = P ("a", P ("b", C (V "a", V "c")));;
check (pgm1);;
check (pgm2);;
check (pgm3);;
check (pgm4);;
check (pgm5);;
check (pgm6);;
check (pgm7);;
