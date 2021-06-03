type nat = ZERO | SUCC of nat

let one = SUCC ZERO
let two = SUCC (SUCC ZERO)
let three = SUCC (SUCC (SUCC ZERO))

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
  match n1 with
    ZERO -> n2
    |SUCC n1' -> SUCC (natadd n1' n2);;
   
let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
  let multiplier = n2 in
  let rec repeat n1 n2 =
    match n1 with
      ZERO -> ZERO
      |SUCC ZERO -> n2
      |SUCC n1' -> repeat n1' (natadd n2 multiplier)
  in
  repeat n1 n2;;
    
natadd two three;;
natadd ZERO two;;

natmul ZERO ZERO;;
natmul ZERO two;;
natmul one three;;
natmul one two;;
natmul three three;;
natmul three two;;    
  