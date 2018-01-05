type nat = ZERO | SUCC of nat
let rec natadd ((a , b) : nat * nat) =
  match (a,b) with
  | (a , ZERO) -> a
  | (a, SUCC ( c : nat)) -> SUCC (natadd (a , c))

let rec natmul ((a , b) : nat * nat) =
  match (a,b) with
  | (a , ZERO) -> ZERO
  | (a , SUCC (c : nat)) -> natadd (a, natmul(a,c))