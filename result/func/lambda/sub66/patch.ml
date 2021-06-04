type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check_list ((lambda : lambda), (lst : string list)) : bool =
  match lambda with
  | V var -> if List.mem var lst then true else false
  | P (var, lambda) -> check_list (lambda, lst @ [ var ])
  | C (lambda1, lambda2) ->
      check_list (lambda1, lst) && check_list (lambda2, lst)


let check (e : lambda) : bool = check_list (e, [])
