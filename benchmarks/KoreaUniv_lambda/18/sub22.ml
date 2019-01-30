type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let check : lambda -> bool
= fun lam ->
  let rec list_search : var list -> var -> bool
  = fun lst v ->
    match lst with 
      | [] -> false
      | hd::tl -> 
        if hd = v then true
        else list_search tl v
        in
  let rec trace : lambda -> var list -> bool
  =fun la var_lst ->
    match la with
      | V v -> 
        if (list_search var_lst v) then true
        else false
      | P (v, nla) ->
        let new_lst = v :: var_lst in
        trace nla new_lst
      | C (nla1, nla2) -> trace nla1 var_lst && trace nla2 var_lst
      in
  trace lam [];;
  
