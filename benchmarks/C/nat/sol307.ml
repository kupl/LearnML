type nat = ZERO | SUCC of nat;;

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->match n1 with
      SUCC a -> natadd a (SUCC n2)
      | ZERO -> n2
;;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
   match n1 with
      SUCC a -> natadd (natmul a n2) n2
      | ZERO -> ZERO
;;

let one_ =  SUCC ZERO;;
let two = SUCC (SUCC ZERO);;
let three = SUCC (SUCC (SUCC ZERO));;

(*TEST CASE*)
(*natadd one_ three ;;*)
(*natmul one_ three ;;*)

