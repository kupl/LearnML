type nat =
ZERO
| SUCC of nat

let rec natadd (n1, n2) =
match n1 with
ZERO -> n2
| SUCC n1 -> natadd (n1, (SUCC n2))

let natmul (n1, n2) =
let rec natmul1 (n1, n2) count ans =
if count = n1 then ans
else natmul1 (n1, n2) (SUCC count) (natadd (ans, n2)) in
natmul1 (n1, n2) ZERO ZERO
