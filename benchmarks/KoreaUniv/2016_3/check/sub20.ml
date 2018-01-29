
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string
  
  let rec isin : var * var list -> bool
  =fun (str,lst) -> match lst with
  | [] -> false
  | hd::tl -> if hd=str then true else isin (str,tl)
  
  let rec filter lst lst2 = 
  match lst with
  | [] -> true
  | hd::tl -> if (isin (hd,lst2) = true) then (filter tl lst2) else false

  let rec bool_check : exp * var list -> var list
  =fun (exp,lst) -> match exp with
  | V str ->  lst
  | P (e1,e2) -> bool_check (e2,[e1] @ lst)
  | C (e1,e2) -> (bool_check (e1, lst)) @ (bool_check (e2, lst))
  
  let rec cal_check : exp * var list -> var list
  =fun (exp,lst) -> match exp with
  | V str -> str::lst
  | P (e1,e2) -> cal_check (e2, lst)
  | C (e1,e2) -> (cal_check (e1,lst)) @ (cal_check (e2, lst))
  
  let check : exp -> bool
  = fun exp -> if ((filter (cal_check (exp,[])) (bool_check (exp,[])))=true) then true else false
