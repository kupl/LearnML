exception NotImplemented

type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec varOfExp : lambda -> var =
 fun lambda ->
  match lambda with
  | V var -> var
  | P (var, e) -> varOfExp e
  | C (e1, e2) -> varOfExp e2


let rec check : lambda -> bool =
 fun lambda ->
  match lambda with
  | P (var1, P (var2, e)) ->
      if varOfExp e = var2 || varOfExp e = var1 then true else false
  | P (var1, C (e1, e2)) -> if varOfExp e2 = var1 then true else false
  | P (var1, V var2) -> if var1 = var2 then true else false
  | _ -> raise NotImplemented
