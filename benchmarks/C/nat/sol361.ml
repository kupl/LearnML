type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
  match n1 with
    |SUCC n -> natadd n (SUCC(n2))
    |_ -> n2 ;;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
  match n1 with
    |SUCC n -> natadd (natmul n n2) n2
    |_ -> ZERO ;;

let two = SUCC (SUCC ZERO);;
let three = SUCC (SUCC (SUCC ZERO));;
natadd two three ;;
natmul two three ;;