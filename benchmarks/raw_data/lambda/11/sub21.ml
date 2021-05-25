type lambda = V of var
| P of var * lambda
| C of lambda * lambda

and var = string

let rec sub (l,a) =
match l with
[] -> []
| hd::tl -> if hd = a then sub(tl,a) else hd::sub(tl,a)

let rec concat (a,b) =
match a with
[] -> b
| hd::tl -> hd::concat(tl,b)

let rec listsub (m:lambda) =
match m with
 V a -> [a]
| P (a,b) -> sub(listsub(b), a)
| C (a,b) -> concat(listsub(a),listsub(b))

let check (m:lambda) =
match m with
 V a -> false
| P (a,b) -> if sub(listsub(b), a) = [] then true else false
| C (a,b) -> false





