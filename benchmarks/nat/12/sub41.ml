type nat = ZERO | SUCC of nat

let rec natadd (nat1, nat2) =
match nat1, nat2 with
|ZERO, nat -> nat
|nat, ZERO -> nat
|SUCC n1, SUCC n2 -> SUCC (natadd (n1, nat2))


let rec natmul (nat1, nat2) =
match nat1, nat2 with
|ZERO, _ -> ZERO
|_, ZERO -> ZERO
|SUCC n1, SUCC n2 -> natadd (nat1, (natmul (nat1, n2)))
