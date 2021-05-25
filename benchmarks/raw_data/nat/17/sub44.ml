(* problem 2*)
type nat = ZERO | SUCC of nat

let natadd : nat -> nat -> nat 
= fun n1 n2 ->
  let rec plus a1 a2 =
    match a1 with
    |ZERO -> (match a2 with
             |ZERO -> ZERO
             |SUCC a -> SUCC (plus a1 a))
    |SUCC b -> SUCC(plus b a2)
  in plus n1 n2;;

let natmul : nat -> nat -> nat 
= fun n1 n2 ->
  let rec mul a1 =
    match a1 with
    |ZERO -> ZERO
    |SUCC a -> natadd n2 (mul a)
  in mul n1;;

let natexp : nat -> nat -> nat 
= fun n1 n2 ->
  let rec exp a1 =
    match a1 with
    |ZERO -> SUCC ZERO
    |SUCC a -> natmul n1 (exp a)
  in exp n2;;
