
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

  let rec set_var : lambda -> var list -> bool
  = fun lambda str -> match lambda with 
  | V r -> if List.mem r str then true else false
  | P (alambda, e) -> set_var e ([alambda;]@str)
  | C (e1, e2) -> set_var e1 str && set_var e2 str

  let rec check : lambda -> bool
  = fun lambda -> match lambda with
  | V str -> false
  | P (str, e) -> set_var e [str;]
  | C (e1, e2) -> check e1 && check e2 (* TODO *)
