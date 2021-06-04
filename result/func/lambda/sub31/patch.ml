type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec checking (mtr : lambda) (arealst : string list) : bool =
  match mtr with
  | V n -> if List.mem n arealst = true then true else false
  | C (m, n) -> checking m arealst && checking n arealst
  | P (n, m) -> checking m (n :: arealst)


let rec check (mtr : lambda) : bool =
  match mtr with
  | V n -> false
  | P (n, m) -> checking mtr []
  | _ -> checking mtr []
