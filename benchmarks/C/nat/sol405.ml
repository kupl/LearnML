type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> (*TODO*)
 match n1 with 
   |ZERO -> n2
   |SUCC nat-> if n2 = ZERO then n1 else SUCC((natadd nat n2));;
   
   
let rec natmul : nat -> nat -> nat
= fun n1 n2 -> (*TODO*)
 match n1 with 
   |ZERO -> ZERO
   |SUCC nat-> if n2 = ZERO then ZERO else (natadd (natmul nat n2) n2);;  
   
let two = SUCC (SUCC ZERO);;

let three = SUCC (SUCC (SUCC ZERO));;

natmul three three;;
natadd two three;;
