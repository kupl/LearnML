type nat = ZERO | SUCC of nat

let natadd : nat -> nat -> nat
= fun n1 n2 -> let rec add n1 n2 k =
 if k = n2 then n1 else SUCC (add n1 n2 (SUCC(k)))
  in add n1 n2 ZERO;;
  
let natmul : nat -> nat -> nat
= fun n1 n2 -> let rec mul n1 n2 k = 
  if k = n2 then ZERO else natadd n1 (mul n1 n2 (SUCC(k)))
   in mul n1 n2 ZERO;;
   
let two = SUCC (SUCC ZERO);;
let three = SUCC (two);;
natadd three three;;
natmul two three;;