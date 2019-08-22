type var = string
type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda

let rec check lambda env =
  match lambda with
  | V s ->
      (match env with
      | h::t -> if h=s then true else (check lambda t)
      | [] -> false)
  | P (n, m) -> (check m (n::env))
  | C (m1, m2) -> (check m1 env) && (check m2 env)

let rec check lambda =
  (let areal = [] in
  match lambda with
  | V s -> false
  | _ -> (check lambda areal))

