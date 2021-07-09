type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check (lam : lambda) : bool =
  let rec find (x : string) (env : string list) : bool =
    match env with
    | [] -> false
    | hd :: tl -> if hd = x then true else find x tl
  in

  let rec del (x : string) (env : string list) : string list =
    match env with
    | [] -> []
    | v :: tl -> if v = x then del x tl else v :: del x tl
  in

  let rec free (l : lambda) (env : string list) : string list =
    match l with
    | V v -> if find v env then env else v :: env
    | P (v, l1) -> del v (free l1 env)
    | C (l1, l2) ->
        let env1 : string list = free l1 env in
        free l1 env @ free l2 env
  in

  let env0 : string list = [] in
  if free lam env0 = [] then true else false
