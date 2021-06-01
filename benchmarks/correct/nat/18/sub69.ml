type nat = ZERO | SUCC of nat

let natadd : nat -> nat -> nat
= fun n1 n2 -> 
  let rec add n1 n2 k = if n2=k then n1 else add (SUCC n1) n2 (SUCC k)
  in add n1 n2 ZERO;;

let natmul : nat -> nat -> nat
= fun n1 n2 -> 
  let rec mul n1 n2 k product = if n2=k then product else mul n1 n2 (SUCC k) (natadd product n1)
  in mul n1 n2 ZERO ZERO;;
