type nat = ZERO | SUCC of nat

let natadd : nat -> nat -> nat
= fun n1 n2 -> 
  let rec f a result
  = match a with
    | ZERO -> result
    | SUCC rest -> f rest (SUCC (result))
  in f n1 n2;;

let natmul : nat -> nat -> nat
= fun n1 n2 ->
  if n1 = ZERO || n2 = ZERO then ZERO
  else
    let rec f a result
    = match a with
      | ZERO -> ZERO
      | SUCC ZERO -> result
      | SUCC rest -> f rest (natadd n2 result)
    in f n1 n2;;
