type lambda = V of var
| P of var * lambda
| C of lambda * lambda
and var = string

let rec check mtr=

let rec cM mtr lst=
match mtr with
| V a -> List.mem a lst
| P (a, m) -> (cM m (a::lst))
| C (a, b) -> (cM a lst) && (cM b lst)
in

(cM mtr [])
;;