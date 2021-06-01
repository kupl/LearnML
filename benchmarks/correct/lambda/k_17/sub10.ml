(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec search_var_list : var list -> var -> bool
= fun vl v ->
  match vl with
  | hd::tl -> if hd = v then true else search_var_list tl v
  | _ -> false

let rec check_rec : lambda -> var list -> bool
= fun lam vl ->
  match lam with
  | V v -> if search_var_list vl v then true else false
  | P (v, l) -> (
    let new_vl = v::vl in
    check_rec l new_vl
  )
  | C (l1, l2) -> (check_rec l1 vl) && (check_rec l2 vl)

let check : lambda -> bool
= fun lam -> 
  let var_list = [] in
  check_rec lam var_list
