type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec checklist : string list * string -> bool =
 fun (l, s) ->
  match l with [] -> false | h :: t -> if h = s then true else checklist (t, s)


let rec extend : lambda * string list -> string list =
 fun (x, env) ->
  match x with
  | V var -> if checklist (env, var) then env else var :: env
  | P (v, e) -> extend (e, env)
  | C (e1, e2) -> extend (e1, extend (e2, env))


let rec check : lambda -> bool =
 fun e ->
  match e with
  | V var -> true
  | P (v, e1) -> check e1 && checklist (extend (e1, []), v)
  | C (e1, e2) -> check e1 && check e2
