type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with
  |ZERO -> n2
  |SUCC nat -> natadd nat (SUCC n2);;
  
let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n1 with
  |ZERO -> ZERO
  |SUCC ZERO -> n2
  |SUCC nat -> let ans = natmul nat n2 in natadd ans n2;;