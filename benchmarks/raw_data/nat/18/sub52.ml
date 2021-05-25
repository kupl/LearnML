type nat = ZERO | SUCC of nat

let natadd : nat -> nat -> nat
= fun n1 n2 -> (*TODO*)
let rec acount=fun a1 a2->
  if a1<>n1 then acount (SUCC a1) (SUCC a2) else a2 in acount ZERO n2;;
  
let natmul : nat -> nat -> nat
= fun n1 n2 -> (*TODO*)
let rec acount=fun a1 a2->
  if a2<>n2 then acount (natadd a1 n1) (SUCC a2) else a1 in acount ZERO ZERO;;


let two = SUCC (SUCC ZERO);;
let three = SUCC (SUCC (SUCC ZERO));; 
natadd two three;;
natmul two three;;
