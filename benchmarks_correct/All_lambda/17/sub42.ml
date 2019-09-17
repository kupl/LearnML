type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
and var = string

let check (m: lambda) : bool =
  let rec helper ((met: lambda), (l: var list)) : bool =
    match met with
    | V x -> List.mem x l
    | P (x, y) -> helper (y, x::l)
    | C (y, z) -> helper (y, l) && helper (z, l)
  in
  helper (m, [])