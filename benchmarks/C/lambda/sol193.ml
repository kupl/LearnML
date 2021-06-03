type lambda = V of var
| P of var * lambda
| C of lambda * lambda
and var = string

let check met : bool =
(*let arealst = getarea met in*)
let rec checkrec met lst : bool =
match met with
  | V a -> if List.mem a lst = true then true else false
  | P (a,b) -> checkrec b (a::lst)
  | C (a,b) -> (checkrec a lst) && (checkrec b lst) in
checkrec met []

