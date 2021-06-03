type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
  match n1 with
  | ZERO -> n2
  | SUCC n -> natadd n (SUCC n2)
;;

let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
  match n1 with
  | ZERO -> ZERO
  | SUCC ZERO -> n2
  | SUCC n -> natmul n (natadd n2 n2)
;;

(*
let zero = ZERO;;
let one = SUCC ZERO;;
let two = SUCC (SUCC ZERO);;
let three = SUCC (SUCC (SUCC ZERO));;

natmul two zero;;
natmul two one;;
natmul two three;;
natadd three zero;;
natadd two three;;
*)