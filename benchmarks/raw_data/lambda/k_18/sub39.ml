type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

type lambda_env = var list

let rec ifinlist : var -> lambda_env -> bool
= fun x l_e ->
  match l_e with
    | [] -> false
    | hd::tl ->
      begin
      match hd with
        |var_in_env -> if x = var_in_env then true else ifinlist x tl
      end

let rec check_dd: lambda -> lambda_env -> bool
= fun lam l_e ->
  match lam with
    | V x -> ifinlist x l_e
    | P (x, l) -> (check_dd l (x::l_e))
    | C (l1, l2) -> check_dd l1 l_e && check_dd l2 l_e;;

let check : lambda -> bool
= fun lam -> check_dd lam [];;

