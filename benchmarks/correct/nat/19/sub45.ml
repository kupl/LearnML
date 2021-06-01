type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with
  | ZERO ->  n2
  | SUCC z -> natadd z (SUCC n2);;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> let rec natmul2 n1 n2 temp = match n1 with
  | ZERO -> ZERO
  | SUCC(ZERO) -> n2
  | SUCC z -> natmul2 z (natadd n2 temp) temp
  in natmul2 n1 n2 n2;;
let five = SUCC(SUCC(SUCC(SUCC(SUCC ZERO))));;
natmul five five;;

