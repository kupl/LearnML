type nat = ZERO | SUCC of nat

let rec natadd (n1 : nat) (n2 : nat) : nat =
  match n1 with ZERO -> n2 | SUCC a -> SUCC (natadd a n2)


let natmul (n1 : nat) (n2 : nat) : nat =
  let rec mul (x : nat) (y : nat) : nat =
    match y with ZERO -> y | SUCC a -> natadd x (mul a x)
  in
  mul n1 n2
