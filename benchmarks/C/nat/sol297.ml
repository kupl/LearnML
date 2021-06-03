type nat = ZERO | SUCC of nat

let natadd : nat -> nat -> nat
= fun n1 n2 -> (*TODO*)
  let rec adder n1 n2 n =
    if n = n1 then n2 
    else SUCC (adder n1 n2 (SUCC n)) in adder n1 n2 ZERO;;

let natmul : nat -> nat -> nat
= fun n1 n2 -> (*TODO*)
  let rec muler n1 n2 n =
    if n = n1 then ZERO
    else natadd n2 (muler n1 n2 (SUCC n)) in muler n1 n2 ZERO;;
