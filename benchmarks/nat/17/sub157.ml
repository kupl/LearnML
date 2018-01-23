type nat = ZERO | SUCC of nat
let rec natadd ((a:nat) ,(b:nat)) =
  match (a,b) with
  | (a , ZERO) -> a
  | (a, SUCC ( c : nat)) -> SUCC (natadd (a , c))

let rec natmul ((a:nat) ,(b:nat)) =
  match (a,b) with
  | (a , ZERO) -> ZERO
  | (a , SUCC (c : nat)) -> natadd (a, natmul(a,c))
