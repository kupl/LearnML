type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check (lam : lambda) : bool =
  match lam with
  | V v1 -> true
  | P (v2, lam1) -> (
      match lam1 with
      | V v2 -> true
      | P (v3, lam2) -> check lam2
      | C (lam3, lam4) ->
          let bool1 : bool = check lam3 in

          let bool2 : bool = check lam4 in
          if bool1 = bool2 = true then true else false
      | C (lam1, lam2) ->
          let bool3 : bool = check lam1 in

          let bool4 : bool = check lam2 in
          if bool3 = bool4 = true then true else false
      | _ -> false )
