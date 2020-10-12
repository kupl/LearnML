type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
and var = string

let check : lambda -> bool = fun m ->
  let rec list_check : string list * string -> bool = fun (l, s) ->
    match l with
    | [] -> false
    | head :: tail ->
      if(head = s) then true
      else list_check(tail, s) in
  let rec r_checklambda : lambda * string list -> bool = fun (m, l) ->
    match m with
    | V var -> list_check(l, var)
    | P (var, lambda) -> r_checklambda(lambda, var :: l)
    | C (lambda1, lambda2) -> r_checklambda(lambda1, l) && r_checklambda(lambda2, l) in
  r_checklambda(m, [])
