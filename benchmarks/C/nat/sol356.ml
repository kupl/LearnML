type nat = ZERO | SUCC of nat

let rec natint n  = match n with
  |ZERO -> 0
  |SUCC n -> 1 + natint (n);;

let rec intnat n  = match n with
  |0 -> ZERO
  |n -> SUCC(intnat (n-1));;


let natadd : nat -> nat -> nat
= fun n1 n2 -> intnat (natint n1 + natint n2);;

let natmul : nat -> nat -> nat
= fun n1 n2 -> intnat (natint n1 * natint n2);;



let two = SUCC (SUCC ZERO);;
let three = SUCC (SUCC (SUCC ZERO));;

natadd two three;;
natmul two three;;
