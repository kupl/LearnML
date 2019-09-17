type lambda = V of var
| P of var * lambda
| C of lambda * lambda
and var = string


let check (m:lambda):bool =
  let rec withList ((l:string list), (mm:lambda)):bool =
    match mm with
    |V(var) -> List.mem var l
    |P(id, m1) -> withList(id::l,m1)
    |C(m1, m2)-> withList(l, m1) && withList(l, m2)
  in
  withList([], m)
