type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string
and env = var list

let ext_env : var -> env -> env
= fun v e -> v::e;;

let rec get_env : var -> env -> bool
= fun v e -> match e with
  | hd::tl -> if hd = v then true else (get_env v tl)
  | [] -> false;;

let rec check_rec : lambda -> env -> bool
= fun lam e -> match lam with
  | V v -> if (get_env v e) then true else false
  | P (v, l) -> check_rec l (ext_env v e)
  | C (l1, l2) -> (check_rec l1 e) && (check_rec l2 e);;

let check : lambda -> bool
= fun lam -> check_rec lam [];;

let lam1 = P ("a", V "a");;
let lam2 = P ("a", P ("a", V "a"));;
let lam3 = P ("a", P ("b", C (V "a", V "b")));;
let lam4 = P ("a", C (V "a", P ("b", V "a")));;
let lam5 = P ("a", V "b");;
let lam6 = P ("a", C (V "a", P ("b", V "c")));;
let lam7 = P ("a", P ("b", C (V "a", V "c")));;

check lam1;;
check lam2;;
check lam3;;
check lam4;;
check lam5;;
check lam6;;
check lam7;;