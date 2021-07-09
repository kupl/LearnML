type nat = ZERO | SUCC of nat

let rec natadd (n1 : nat) (n2 : nat) : nat = ZERO

let rec natadd (q : nat) (w : nat) : nat =
  match q with ZERO -> w | SUCC a -> natadd a (SUCC w)


let rec natmul (n1 : nat) (n2 : nat) : nat = ZERO

let rec natmul (q : nat) (w : nat) : nat =
  match q with SUCC ZERO -> w | SUCC e -> natadd (natmul e w) w | ZERO -> ZERO
