type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string
;;

let rec bound_var_list lamlist lst =
  match lamlist with
  | [] -> true
  | hd :: tl -> (match hd with
                 | V a -> if ((List.mem a lst) = true) then true else false
                 | P (a, l) -> if ((List.mem a lst) = true) then (bound_var_list [l] lst) else (bound_var_list [l] (a :: lst))
                 | C (l1, l2) -> if ((bound_var_list [l1] lst) = true) then (bound_var_list [l2] lst) else false
                 )
;;

let check : lambda -> bool
= fun lam ->
  bound_var_list [lam] []
;;









