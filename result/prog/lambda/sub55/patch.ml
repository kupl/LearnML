type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec pcheck ((var : string list), (lambda : lambda)) : bool =
  match lambda with
  | V evar -> List.exists (fun (v : string) -> v = evar) var
  | P (v, lambda) -> pcheck (v :: var, lambda)
  | C (e1, e2) -> pcheck (var, e1) && pcheck (var, e2)


let rec check (e : lambda) : bool = pcheck ([], e)
