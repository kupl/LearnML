type nat = ZERO | SUCC of nat

let rec natAdd : nat -> nat -> nat
= fun n1 n2 -> 
  match n1 with
    | ZERO -> n2
    | SUCC n -> natAdd n (SUCC n2);; 
    
let rec natMul : nat -> nat -> nat
= fun n1 n2 ->
  match n1 with
    | ZERO -> ZERO
    | SUCC n -> natAdd n2 (natMul n n2);;
  
  
let natadd : nat -> nat -> nat
= fun n1 n2 -> natAdd n1 n2;;

let natmul : nat -> nat -> nat
= fun n1 n2 -> natMul n1 n2;;

let one = SUCC ZERO;;
let two = SUCC (SUCC ZERO);;
let three = SUCC (SUCC (SUCC ZERO));;
let four = SUCC (SUCC (SUCC (SUCC ZERO)));;
natadd four three;;
natmul three four;;