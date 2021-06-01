type nat = ZERO | SUCC of nat

let natadd : nat -> nat -> nat
= fun n1 n2 -> 
  
  let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> 
  match n2 with
  | ZERO -> n1
  | SUCC n -> natadd (SUCC n1) n in
  natadd n1 n2;;
  
let natmul : nat -> nat -> nat
= fun n1 n2 -> 
  
  let rec natmul : nat -> nat -> nat 
= fun n1 n2 -> 
  match n2 with
  | ZERO -> ZERO
  | SUCC n -> natadd n1 (natmul n1 n) in
  natmul n1 n2;;
  
  let two = SUCC (SUCC ZERO);;
  let three = SUCC (SUCC (SUCC ZERO));;
  
  natmul two three;;