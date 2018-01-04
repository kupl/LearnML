(*컴퓨터공학부 2014-16775 김민지
programming language hw 1-4*)

type nat = ZERO | SUCC of nat

let rec natadd ((x:nat), (y:nat)) : nat = 
  match x with
  |ZERO -> y
  |SUCC(x1) -> natadd(x1, SUCC(y))

let rec natmul ((x:nat), (y:nat)) : nat = 
  match x with
  |ZERO -> ZERO
  |SUCC(x1) -> natadd(natmul(x1, y), y)
