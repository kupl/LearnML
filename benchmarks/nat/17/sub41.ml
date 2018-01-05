type nat = ZERO | SUCC of nat
let rec natadd ((x,y) : nat * nat) : nat =
  match (y) with
  |(ZERO) -> x
  |(SUCC(z)) -> SUCC(natadd(x,z))
let rec natmul ((x,y) : nat * nat) : nat =
  match (y) with
  |(ZERO) -> ZERO
  |(SUCC(z)) -> natadd(x, natmul(x, z))
