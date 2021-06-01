type nat = ZERO | SUCC of nat


let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
  match n1 with
    | ZERO -> n2
    | SUCC n1'-> SUCC (natadd n1' n2) 

let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
  match n1 with
  | ZERO -> ZERO
  | SUCC n1'-> natadd n2 (natmul n1' n2)

let zero=ZERO
let one =SUCC (ZERO)
let two =SUCC (SUCC ZERO)
let three=SUCC (SUCC (SUCC ZERO))
let four=SUCC(SUCC (SUCC (SUCC ZERO)));;

natadd two three;;
natmul two three;;
natmul three four;;