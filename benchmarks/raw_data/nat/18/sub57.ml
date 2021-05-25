type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
  match n1 with ZERO -> n2
  | SUCC p1 -> SUCC (natadd p1 n2)
;;

let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
  match n1 with ZERO -> ZERO
  | SUCC p1 -> natadd n2 (natmul p1 n2)
;;

let two = SUCC (SUCC ZERO);;
let three = SUCC (SUCC (SUCC ZERO));;
natmul two three;;
natadd two three;;
