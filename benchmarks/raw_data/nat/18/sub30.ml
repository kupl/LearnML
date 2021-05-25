type nat = ZERO | SUCC of nat;;

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> (*TODO*)
  match n1 with
    ZERO -> n2
    | SUCC (s)-> natadd s (SUCC n2);;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> (*TODO*)
    if n1=ZERO then ZERO else if n2=ZERO then ZERO else
      match n1 with
        
        ZERO | SUCC(ZERO) -> n2
        |SUCC(s) -> natadd n2 (natmul s n2) ;;


let two = SUCC(SUCC ZERO);;
let three=SUCC(SUCC(SUCC ZERO));;
let four=SUCC(three);;

natadd two four;;
natmul two four;;