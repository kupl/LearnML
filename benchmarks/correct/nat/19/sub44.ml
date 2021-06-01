type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with
   ZERO -> n2
  |SUCC nat -> SUCC (natadd nat n2);;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n1 with
   ZERO -> ZERO
  |SUCC nat -> natadd (natmul nat n2) n2;;


let zer = ZERO;;
let two = SUCC (SUCC ZERO);;
let three = SUCC (SUCC (SUCC ZERO));;
natadd three two;;
natmul three two;;