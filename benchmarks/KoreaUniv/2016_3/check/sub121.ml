
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let rec append_list list1 list2 = 
  match list1 with
    | [] -> list2
    | hd::tl -> hd :: (append_list tl list2);;

  let rec get_variables exp result =
  match exp with
    | V v -> v :: result
    | P (v, e) -> get_variables e (v :: result); 
    | C (e1, e2) -> append_list (get_variables e1 result) (get_variables e2 result)

  let rec var_in_exp variable exp = 
  match exp with
    | V v -> false 
    | P (v, e) -> if v = variable then true else var_in_exp variable e
    | C (e1, e2) -> var_in_exp variable e1 || var_in_exp variable e2

  let rec check_helper variables exp = 
  match variables with
    | [] -> true
    | hd::tl -> if var_in_exp hd exp = false then false else check_helper tl exp

  let check : exp -> bool
  = fun exp -> 
  let variables = get_variables exp [] in
    check_helper variables exp;;
