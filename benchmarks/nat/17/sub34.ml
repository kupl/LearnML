type nat = ZERO
         | SUCC of nat

let rec natadd ((a:nat), (b:nat)) : nat =
  match a with
  |ZERO -> b
  |SUCC n -> natadd (n, SUCC b)

let rec natmul ((a:nat), (b:nat)) : nat =
  match a with
  |ZERO -> ZERO
  |SUCC n -> natadd (b, natmul(n, b))
