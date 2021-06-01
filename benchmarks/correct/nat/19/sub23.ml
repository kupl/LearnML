type nat = ZERO | SUCC of nat

let natadd : nat -> nat -> nat
= fun n1 n2 ->
  let rec plusone nx ny =
    match ny with
      | ZERO -> nx
      | SUCC nz -> SUCC (plusone nx nz)
  in plusone n1 n2;;

let natmul : nat -> nat -> nat
= fun n1 n2 ->
  let rec paste nx ny =
    match ny with
      | ZERO -> ZERO
      | SUCC nz -> let nw = paste nx nz in natadd nw nx
  in paste n1 n2;;