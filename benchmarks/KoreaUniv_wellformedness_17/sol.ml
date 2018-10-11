type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda

and var = string

let rec is_mem : var -> var list -> bool
= fun x l ->
  match l with
  | [] -> false
  | hd::tl -> if (hd = x) then true else is_mem x tl

let rec sub_check : lambda -> var list -> bool
= fun lam env ->
  match lam with
  | V x -> is_mem x env
  | P (x, l) -> sub_check l (x::env)
  | C (l1, l2) -> (sub_check l1 env)&&(sub_check l2 env)

let rec check : lambda -> bool
= fun lam -> sub_check lam []