type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let rec check : lambda -> bool
  = fun lamb ->
  let rec find : var -> var list -> bool
  = fun x lst ->
  match lst with
  |[] -> false
  |hd::tl ->if(hd=x) then true else find x tl in
  let rec check_sub : lambda -> var list -> bool
  = fun lamb lst ->
  match lamb with
  |V v -> find v lst
  |P (v,lam) -> check_sub lam (v::lst)
  |C (lam1,lam2) -> (check_sub lam1 lst)&&(check_sub lam2 lst) in
  check_sub lamb [];;