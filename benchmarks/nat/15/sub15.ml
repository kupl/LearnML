type nat = ZERO | SUCC of nat

let rec natadd (a,b) : nat = match (a,b) with
  (ZERO, ZERO) -> ZERO
| (ZERO, SUCC i) -> SUCC i
| (SUCC i, ZERO) -> SUCC i
| (SUCC i, SUCC j) -> SUCC (natadd(SUCC i, j))

let rec natmul (a, b) : nat = match (a,b) with
  (ZERO, ZERO) -> ZERO
| (ZERO, SUCC i) -> ZERO
| (SUCC i, ZERO) -> ZERO
| (SUCC i, SUCC j) -> natadd(SUCC i, natmul(SUCC i, j))
