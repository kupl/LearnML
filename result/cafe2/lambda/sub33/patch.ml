type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check (met : lambda) : bool = List.length (idList met) = 0

and idList (met : lambda) : string list =
  match met with
  | V n -> [ n ]
  | P (n, metr) -> List.filter (fun (__s9 : string) -> __s9 != n) (idList metr)
  | C (metr1, metr2) -> idList metr1 @ idList metr2


and idSearch ((id : string), (l : string list)) : bool =
  match l with
  | [] -> false
  | head :: tail -> if head = id then true else idSearch (id, tail)
