
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

  let rec is_V_here : string list -> string -> bool
  = fun lst var ->
  (match lst with
  | [] -> false
  | hd::tl -> if hd = var then true else is_V_here tl var)

  let rec check : lambda -> bool
  = fun lambda -> 
  (match lambda with
  | V v -> false
  | P (v, e) -> sub_check [v] e
  | C (e1, e2) -> check e1 && check e2)

  and sub_check : string list -> lambda -> bool
  = fun lst lambda ->
  (match lambda with
  | V v -> is_V_here lst v
  | P (v, e) -> sub_check (v::lst) e
  | C (e1, e2) -> (sub_check lst e1) && (sub_check lst e2))
