type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check (lambda : lambda) : bool =
  let rec varStation (lambda : lambda) : string list =
    match lambda with
    | V n -> [ n ]
    | P (n, m) -> varStation m
    | C (m1, m2) -> List.append (varStation m1) (varStation m2)
  in

  let rec varArea (lambda : lambda) : string list =
    match lambda with
    | V n -> []
    | P (n, m) -> n :: varArea m
    | C (m1, m2) -> List.append (varArea m1) (varArea m2)
  in

  let rec check2 (l1 : string list) (l2 : string list) : bool =
    match l1 with [] -> true | hd :: tl -> List.mem hd l2 && check2 tl l2
  in
  check2 (varStation lambda) (varArea lambda)
