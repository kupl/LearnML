type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> (*TODO*)
  match n1 with
    |ZERO -> n2
    |SUCC n -> SUCC (natadd n n2);;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> (*TODO*)
  match n1 with
    |ZERO -> ZERO
    |SUCC n -> (natadd (natmul n n2) n2);;
    
let two = SUCC (SUCC ZERO);;
let three = SUCC (SUCC (SUCC ZERO));;
natmul two three;;
natadd two three;;
