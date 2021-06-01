type nat = ZERO | SUCC of nat;;
ZERO;;
let three = SUCC(SUCC(SUCC ZERO));;
let two = SUCC(SUCC ZERO);;

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
  match n1 with
    SUCC n1' -> natadd n1' (SUCC n2)
    |ZERO -> n2;;
  
natadd two three;;
natadd two three;;
natadd two ZERO;;
natadd ZERO three;;

let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
  if n1=ZERO || n2=ZERO then ZERO else
    match n1 with
      SUCC n1' -> natadd (natmul n1' n2) n2 
      |ZERO ->n2;;
  
natmul ZERO two;;
natmul ZERO three;;
natmul ZERO two;;
natmul ZERO two;;