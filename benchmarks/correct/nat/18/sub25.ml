type nat = ZERO | SUCC of nat

let natadd : nat -> nat -> nat
= fun n1 n2 -> 
  let rec natadd' n1 n2 =
  match n1 with
    ZERO -> n2
  | SUCC(n') -> SUCC( natadd' n' n2)
  in natadd' n1 n2;;

let natmul : nat -> nat -> nat
= fun n1 n2 -> 
  let rec natmul' n1 n2 = 
    match n1 with
    ZERO  -> ZERO
  | SUCC(n') -> natadd n2 (natmul' n' n2)
  in natmul' n1 n2;;



let two = SUCC (SUCC ZERO);;


let three = SUCC (SUCC (SUCC ZERO));;


natmul two three;;
(*- : nat = SUCC (SUCC (SUCC (SUCC (SUCC (SUCC ZERO)))))*)

natadd two three;;
(*- : nat = SUCC (SUCC (SUCC (SUCC (SUCC ZERO))))*)
