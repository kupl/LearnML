type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check (met : lambda) : bool =
  match met with
  | V n -> false
  | P (n, metr) -> idSearch (n, idList metr) || check metr
  | C (metr1, metr2) -> check metr1 && check metr2


and idList (met : lambda) : string list =
  match met with
  | V n -> [ n ]
  | P (n, metr) -> idList metr
  | C (metr1, metr2) -> idList metr1 @ idList metr2


and idSearch ((id : string), (l : string list)) : bool =
  match l with
  | [] -> false
  | head :: tail -> if head = id then true else idSearch (id, tail)
