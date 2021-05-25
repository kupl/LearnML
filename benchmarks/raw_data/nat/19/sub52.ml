type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
  match n2 with
    | ZERO -> n1
    | SUCC (nat) -> natadd (SUCC n1) nat;;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
    match n2 with
      | ZERO -> ZERO
      | SUCC nat -> natadd n1 (natmul n1 nat);;

let two = SUCC(SUCC ZERO);;
let three = SUCC(SUCC(SUCC ZERO));;

natadd two three;;
natmul two three;;