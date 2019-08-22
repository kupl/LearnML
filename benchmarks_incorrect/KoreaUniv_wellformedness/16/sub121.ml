
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

  let rec append_list list1 list2 = 
  match list1 with
    | [] -> list2
    | hd::tl -> hd :: (append_list tl list2);;

  let rec get_variables lambda result =
  match lambda with
    | V v -> v :: result
    | P (v, e) -> get_variables e (v :: result) 
    | C (e1, e2) -> append_list (get_variables e1 result) (get_variables e2 result)

  let rec var_in_lambda variable lambda = 
  match lambda with
    | V v -> false 
    | P (v, e) -> if v = variable then true else var_in_lambda variable e
    | C (e1, e2) -> var_in_lambda variable e1 || var_in_lambda variable e2

  let rec check_helper variables lambda = 
  match variables with
    | [] -> true
    | hd::tl -> if var_in_lambda hd lambda = false then false else check_helper tl lambda

  let check : lambda -> bool
  = fun lambda -> 
  let variables = get_variables lambda [] in
    check_helper variables lambda;;
