let empty_env' : 'a list = []

let extend_env' (x : lambda) (e : 'b list) : 'b list = x :: e

let rec apply_env' (e : 'c list) x : bool =
  match e with
  | [] -> false
  | y :: tl -> if x = y then true else apply_env' tl x


type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check (lam : lambda) : bool =
  let rec check' (lam : lambda) (env : lambda list) : bool =
    match lam with
    | V x -> apply_env' env lam
    | P (x, f) ->
        let proc : lambda list = extend_env' (V x) env in
        check' f proc
    | C (f1, f2) -> if check' f1 env = check' f2 env then true else false
  in
  check' lam empty_env'
