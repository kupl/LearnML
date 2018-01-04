type nat = ZERO
| SUCC of nat

let rec add ((n1:nat),(n2:nat)) =
match n1 with
ZERO -> n2
| SUCC a -> add (a, (SUCC n2))

let rec mul ((n1:nat),(n2:nat)) =
match n1 with
 ZERO -> ZERO
| SUCC a -> add (n2, (mul (a, n2)))
