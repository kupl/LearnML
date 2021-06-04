type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec pcheck : var list * lambda -> bool =
 fun (var, lambda) ->
  match lambda with
  | V evar -> List.exists (fun v -> v = evar) var
  | P (v, lambda) -> pcheck (v :: var, lambda)
  | C (e1, e2) -> pcheck (var, e1) && pcheck (var, e2)


let rec check : lambda -> bool =
 fun e ->
  match e with
  | V v -> false
  | P (v, lambda) -> pcheck ([ v ], lambda)
  | C (e1, e2) -> false
