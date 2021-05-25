type nat = ZERO | SUCC of nat

let subbing : nat -> nat
= fun n ->
  match n with
    | ZERO -> ZERO
    | SUCC(n2) -> n2;;
    
(*let rec concatenation : nat -> nat -> nat
= fun n1 n2 ->
  match n2 with
    | ZERO -> n1
    | SUCC(n2) -> SUCC (concatenation n1 (subbing n2));;*)

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
  match n2 with
    | ZERO -> n1
    | _ -> SUCC (natadd n1 (subbing n2));;


let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
  match n2 with
    | ZERO -> ZERO
    | SUCC ZERO -> n1
    | SUCC (SUCC ZERO) -> natadd n1 n1
    | _ -> natadd n1 (natmul n1 (subbing n2)) ;;
  
let two = SUCC (SUCC ZERO);;
let three = SUCC (SUCC (SUCC ZERO));;
let four = SUCC (SUCC (SUCC (SUCC ZERO)));;


natadd two three;;
natmul two three;;

natadd three four;;
natmul three four;;