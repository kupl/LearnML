type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check : lambda -> bool =
 fun e ->
  match e with
  | V x -> false
  | P (x, e1) -> (
      match e1 with
      | V y -> x = y
      | P (y, e2) -> (
          match e2 with
          | C (e3, e4) ->
              (check (P (x, e3)) || check (P (x, e4)))
              && (check (P (y, e3)) || check (P (y, e4)))
          | _ -> check (P (x, e2)) || check (P (y, e2)) )
      | C (e2, e3) -> check (P (x, e2)) && check (P (x, e3)) )
  | C (e1, e2) -> check e1 && check e2
