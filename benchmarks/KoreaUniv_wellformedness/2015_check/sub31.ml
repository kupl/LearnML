  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string

  let rec find_exp
  =fun e l -> 
    match l with
    | h::t -> if h=e then true else find_exp e t 
    | [] -> false

  let check : exp -> bool
  =fun e ->
    let var_lst = [] in
    let rec check_el 
    =fun ex lst ->
      match ex with
      | V (v) -> find_exp v lst 
      | P (v,e1) -> check_el e1 (v::lst)
      | C (e1,e2) -> (check_el e1 lst) && (check_el e2 lst)
    in check_el e var_lst 
