exception TODO

type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check (m : lambda) : bool =
  match m with
  | P (id, y) -> (
      if check y then true
      else
        match y with
        | V s -> s = id
        | P (a, metr) -> (
            match metr with
            | C (m1, m2) ->
                (check (P (a, m1)) || check (P (id, m1)))
                && (check (P (a, m2)) || check (P (id, m2)))
            | _ -> check (P (id, metr)) || check (P (a, metr)) )
        | C (m1, m2) -> check (P (id, m1)) && check (P (id, m2)) )
  | C (m1, m2) -> check m1 && check m2
  | _ -> false
