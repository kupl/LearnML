type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
  = fun n1 n2 ->
  match n1 with
  | ZERO -> n2
  | SUCC value -> SUCC (natadd value n2);;

let rec natmul : nat -> nat -> nat
  = fun n1 n2 ->
  match n1 with
  | ZERO -> ZERO
  | SUCC value -> natadd n2 (natmul value n2);;

