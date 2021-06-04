type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec doesExist ((var : string), (env : string list)) : bool =
  match env with
  | [] -> false
  | hd :: tl -> if var = hd then true else doesExist (var, tl)


let rec isBound ((lam : lambda), (env : string list)) : bool =
  match lam with
  | V v -> if doesExist (v, env) then true else false
  | P (v, l) -> isBound (l, env @ [ v ])
  | C (l1, l2) -> isBound (l1, env) && isBound (l2, env)


let rec check (lam : lambda) : bool =
  match lam with
  | V v -> false
  | P (v, l) -> isBound (l, [ v ])
  | C (l1, l2) -> check l1 && check l2
