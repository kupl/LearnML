type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
    match n2 with
      | ZERO -> n1
      | SUCC m -> SUCC (natadd n1 m);;

let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
    match n2 with
      | ZERO -> ZERO
      | SUCC ZERO -> n1
      | SUCC (SUCC m) -> (natadd n1 (natmul n1 (SUCC m)));;

(* Test Cases
let my0 = ZERO;;
let my1 = SUCC ZERO;;
let my2 = SUCC (SUCC ZERO);;
let my3 = SUCC (SUCC (SUCC ZERO));;

natadd my0 my2;;
natadd my2 my3;;

natmul my0 my2;;
natmul my2 my3;;
*)
