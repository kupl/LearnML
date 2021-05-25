type nat = ZERO | SUCC of nat

let natadd : nat -> nat -> nat
= fun n1 n2 -> 
  let rec addition : nat -> nat -> nat
  = fun a b ->
    match b with 
      | ZERO -> a
      | SUCC(x) -> SUCC(addition a x)
  in addition n1 n2;;

let natmul : nat -> nat -> nat
= fun n1 n2 -> 
  let rec multiply : nat -> nat -> nat
  = fun a b ->
    match b with
      | ZERO -> ZERO
      | SUCC(x) -> natadd a (multiply a x)
  in multiply n1 n2;;