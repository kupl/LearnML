type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
  match n1 with
    ZERO -> n2
    |SUCC ( temp ) -> SUCC ( natadd temp n2 );;

let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
  if n1 = ZERO then ZERO
  else
    match n2 with
      ZERO -> ZERO
      |SUCC ZERO -> n1
      |SUCC ( temp ) -> natadd n1 (natmul n1 temp);;

let two = SUCC (SUCC ZERO);;
let three = SUCC (SUCC (SUCC ZERO));;

natmul two three;;
natadd two three;;
