type nat = ZERO | SUCC of nat

let natadd : nat -> nat -> nat
= fun n1 n2 -> let natt x=match x with
  |ZERO->ZERO 
  |SUCC(k)->k 
  in
  let rec natn1 n1= if n1= ZERO then ZERO else SUCC (natt n1) in
  let rec natn2 n2= if n2= ZERO then natn1 n1 else SUCC (natn2 (natt n2)) in natn2 n2;;
let natmul : nat -> nat -> nat
= fun n1 n2 -> let rec mult n1 n2 =match n1 with
  |ZERO -> ZERO 
  |SUCC(k)-> natadd n2 (mult k n2) in mult n1 n2;;

let two = SUCC (SUCC ZERO);;
let three = SUCC(SUCC(SUCC ZERO));;

natadd two three;;
natmul two three;;


