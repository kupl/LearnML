type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check (met : lambda) : bool =
  let rec checkArea (id : string list) (m : lambda) : bool =
    match m with
    | V n -> List.exists (fun (x : string) -> x = n) id
    | C (s1, s2) -> checkArea id s1 && checkArea id s2
    | P (id2, m) -> checkArea (id2 :: id) m
  in

  match met with P (id, m) -> checkArea [ id ] m | _ -> false
