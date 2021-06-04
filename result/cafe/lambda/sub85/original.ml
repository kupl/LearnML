type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check (lambda : lambda) : bool =
  match lambda with
  | P (s, V a) -> if s = a then true else false
  | P (s, P (s1, e1)) -> (
      match e1 with
      | C (e2, e3) ->
          (check (P (s, e2)) || check (P (s1, e2)))
          && (check (P (s, e3)) || check (P (s1, e3)))
      | _ ->
          if check (P (s1, e1)) = true || check (P (s, e1)) then true else false
      )
  | P (s, C (e1, e2)) -> (
      match (e1, e2) with
      | P (s1, e3), e ->
          if check (P (s, P (s1, e3))) && check (P (s, P (s1, e))) then true
          else false
      | e, P (s1, e3) ->
          if check (P (s, P (s1, e3))) && check (P (s, P (s1, e))) then true
          else false
      | _ -> if check (P (s, e1)) && check (P (s, e2)) then true else false )
  | _ -> false
