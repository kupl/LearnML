type nat = ZERO | SUCC of nat

let natadd : nat -> nat -> nat
= fun n1 n2 ->let rec add_nat a b = match a 
with ZERO -> b 
| SUCC(temp_nat) -> SUCC(add_nat temp_nat b) in add_nat n1 n2;;


let natmul : nat -> nat -> nat
= fun n1 n2 -> let rec mul_nat a b = match a 
with ZERO -> ZERO 
| SUCC(temp_nat) -> natadd b (mul_nat temp_nat b) in mul_nat n1 n2;;


let two = SUCC (SUCC ZERO);;
let three = SUCC (SUCC (SUCC ZERO));;

natadd two three;;
natmul three two;;
natmul two three;;
