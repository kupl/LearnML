type nat = ZERO | SUCC of nat

let rec natadd (n1 : nat) (n2 : nat) : nat = ZERO

let rec natadd (n1 : nat) (n2 : nat) : nat =
  match n1 with ZERO -> n2 | SUCC h -> natadd h (SUCC n2)


let rec natmul (n1 : nat) (n2 : nat) : nat = ZERO

let rec natmul (n1 : nat) (n2 : nat) : nat =
  match n1 with ZERO -> SUCC n2 | SUCC h -> natmul h (SUCC n2)
