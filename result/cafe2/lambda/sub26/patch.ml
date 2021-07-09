type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let check (m : lambda) : bool =
  let rec checkRec (m : lambda) (s : var list) : bool =
    match m with
    | V n -> List.mem n s
    | P (n, m) -> checkRec m (n :: s)
    | C (m1, m2) -> checkRec m1 s && checkRec m2 s
  in
  checkRec m []
