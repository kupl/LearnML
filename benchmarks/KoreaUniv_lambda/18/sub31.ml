type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let rec contains l x =
  match l with
    [] -> false
  | h::t -> if h = x then true else contains t x

let rec check_inner bound_var prog =
  match prog with
    V var -> if contains bound_var var then true else false
  | P (v, e) -> check_inner (v::bound_var) e
  | C (e1, e2) -> check_inner bound_var e1 && check_inner bound_var e2

let check : lambda -> bool
= fun lam -> check_inner [] lam;;

(* well-formed *)
check (P ("a", V "a"));;
check (P ("a", P ("a", V "a")));;
check (P ("a", P ("b", C (V "a", V "b"))));;
check (P ("a", C (V "a", P ("b", V "a"))));;
check (P ("x", P ("y", C (C (V "y", V "y"), V "x"))));;
check (C (P ("x", V "x"), P ("y", V "y")));;
check (P ("x", P ("y", C (V "x", V "y"))));;

(* ill-formed *)
check (P ("a", V "b"));;
check (P ("a", C (V "a", P ("b", V "c"))));;
check (P ("a", P ("b", C (V "a", V "c"))));;
check (P ("x", C (V "x", P ("z", C (V "x", V "y")))));;
check (C (V "x", V "y"));;
check (V "x");;
check (C (V "y", P ("x", C (V "x", V "z"))));;