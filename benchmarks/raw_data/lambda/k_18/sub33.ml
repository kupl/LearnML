type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string


(* ----------------------------- my code start ------------------------------ *)

let rec findenv env x = match env with
  | [] -> false
  | hd::tl -> if hd = x then true else findenv tl x

let rec lamchk lam env = match lam with
  | V x -> findenv env x
  | P (px, pl) -> lamchk pl (px::env)
  | C (cl1, cl2) -> lamchk cl1 env && lamchk cl2 env

(* ----------------------------- my code end------------------------------ *)


let check : lambda -> bool
= fun lam -> lamchk lam []

let pgm1 = P ("a", V "a")
let pgm2 = P ("a", P ("a", V "a"))
let pgm3 = P ("a", P ("b", C (V "a", V "b")))
let pgm4 = P ("a", C (V "a", P ("b", V "a")))

let pgm5 = P ("a", V "b")
let pgm6 = P ("a", C (V "a", P ("b", V "c")))
let pgm7 = P ("a", P ("b", C (V "a", V "c")))

let test1 = C (P ("x", P ("y", V "x")), V "y");;

check pgm1;;
check pgm2;;
check pgm3;;
check pgm4;;
check pgm5;;
check pgm6;;
check pgm7;;

check test1;;