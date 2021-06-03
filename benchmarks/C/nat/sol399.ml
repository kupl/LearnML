type nat = ZERO | SUCC of nat
let zero = ZERO
let one = SUCC zero
let two = SUCC one
let three = SUCC two

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with
  | ZERO -> n2
  | SUCC n1 -> SUCC(natadd n1 n2);;

let rec natsub : nat -> nat -> nat 
= fun num oldnum ->
  if SUCC num = oldnum then num
  else natsub (SUCC num) oldnum;;

let rec natmul : nat -> nat -> nat
  = fun n1 n2 -> 
  match n2 with
    | ZERO -> ZERO
    |_ -> natadd n1 (natmul n1 (natsub ZERO n2));;
natadd three two;;
natmul three two;;
  


  
  
