type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat = fun n1 n2 ->
  match n1 with
    |ZERO -> n2
    |SUCC a -> SUCC (natadd a n2);;

let natmul : nat -> nat -> nat = fun n1 n2 ->
  let rec mul x y =
    match x with
      |ZERO -> y
      |SUCC a -> mul a (natadd n2 n2)
    in mul n1 n2;;