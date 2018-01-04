type nat = ZERO | SUCC of nat

let rec natadd (n, m) = match m with
	ZERO -> n
  | SUCC m1 -> SUCC (natadd (n, m1))

let rec natmul (n, m) = match m with
	ZERO -> ZERO
  | SUCC m1 -> natadd (natmul (n, m1), n)