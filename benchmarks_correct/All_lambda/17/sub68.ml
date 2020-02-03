type var = string
type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda

let rec check2 lambda env =
  match lambda with
  | V s ->
      (match env with
      | h::t -> if h=s then true else (check2 lambda t)
      | [] -> false)
  | P (n, m) -> (check2 m (n::env))
  | C (m1, m2) -> (check2 m1 env) && (check2 m2 env)

let rec check lambda =
  (let areal = [] in
  match lambda with
  | V s -> false
  | _ -> (check2 lambda areal))