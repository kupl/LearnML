type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec station_list (x : lambda) : string list =
  match x with
  | V n -> [ n ]
  | P (n, m) -> List.filter (fun (__s8 : string) -> __s8 != n) (station_list m)
  | C (m1, m2) -> List.append (station_list m1) (station_list m2)


let rec check (x : lambda) : bool = List.length (station_list x) = 0
