type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with 
  | ZERO -> n2
  | SUCC ZERO -> if n2 = ZERO then n1 else SUCC(n2)
  | SUCC(n1') -> if n2 = ZERO then n1 else natadd n1' (SUCC(n2));;


let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n1 with
  | ZERO -> ZERO
  | SUCC ZERO -> n2
  | SUCC(n1') -> natadd n2 (natmul n1' n2);;


let two = SUCC (SUCC ZERO);;

let three = SUCC (SUCC (SUCC ZERO));;


natadd two three;;

natmul two three;;

