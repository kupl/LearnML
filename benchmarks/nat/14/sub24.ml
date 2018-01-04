(*컴퓨터공학부 2010-11779 박진영 1.3*)
type nat = ZERO | SUCC of nat

let rec nattoint n i =
  match n with
  | ZERO -> i
  | SUCC nat -> nattoint nat (i+1)

let rec inttonat i n =
  if i <= 0 then n
  else inttonat (i-1) (SUCC n)

let natadd (n1, n2) =
match (n1, n2) with
| (ZERO, _) -> n2
| (_, ZERO) -> n1
| (SUCC n11, SUCC n22) ->
  inttonat (nattoint n1 0 + nattoint n2 0) ZERO

let natmul (n1, n2) =
match (n1, n2) with
| (ZERO, _) -> ZERO
| (_, ZERO) -> ZERO
| (SUCC n11, SUCC n22) ->
  inttonat (nattoint n1 0 * nattoint n2 0) ZERO


