type nat = ZERO | SUCC of nat

let two = SUCC(SUCC ZERO);;

let three = SUCC(SUCC(SUCC ZERO));;

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
  match n1 with
    | ZERO -> n2
    | SUCC m -> SUCC(natadd m n2);;
    
let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
  match n1 with
    | ZERO -> ZERO
    | SUCC m -> natadd n2 (natmul m n2);; 
    
natadd three three;;
natmul two ZERO;;