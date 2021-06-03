type nat = ZERO | SUCC of nat;;

let two = SUCC (SUCC ZERO);;
let three = SUCC (SUCC (SUCC ZERO));;


let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with
  ZERO -> n2
  |SUCC x -> natadd x (SUCC n2);;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n1 with
  ZERO -> n1
  |SUCC x -> natadd n2 (natmul x n2);;

natmul two three;;
natadd two three;;