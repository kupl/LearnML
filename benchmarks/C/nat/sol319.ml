type nat = ZERO | SUCC of nat;;

let two = SUCC (SUCC ZERO);;
let three = SUCC (SUCC (SUCC ZERO));;

let natadd : nat -> nat -> nat
= fun n1 n2 ->
    let rec inner_natadd n1 n2 = 
      match n1 with 
        | ZERO -> n2
        | SUCC(to_decrement) -> inner_natadd to_decrement (SUCC n2) 
    in inner_natadd n1 n2;;
    

let natmul : nat -> nat -> nat
= fun n1 n2 -> 
    let rec inner_natmul n1 n2 = 
      match n1 with 
        | ZERO -> ZERO
        | SUCC(to_decrement) -> natadd n2 (inner_natmul to_decrement n2) 
    in inner_natmul n1 n2;;


natadd two three;;
natmul two three;;
