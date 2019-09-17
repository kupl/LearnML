type lambda = V of var | P of var * lambda | C of lambda * lambda and var = string

let rec check2 : (lambda * string list) -> bool = fun (m,s) -> match m with
  |V k -> if List.mem k s then true else false
  |P (k,q) -> check2 (q, k::s)
  |C (k,q) -> check2 (k,s) && check2 (q,s)

let rec check m = check2 (m, [])


