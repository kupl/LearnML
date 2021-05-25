type nat = ZERO | SUCC of nat

let rec my_natadd n1 n2 x =
  if x = n1 then n2
  else SUCC (my_natadd n1 n2 (SUCC x));;

let natadd : nat -> nat -> nat
= fun n1 n2 -> my_natadd n1 n2 ZERO;;

let rec my_natmul n1 n2 x =
  if x = n1 then ZERO
  else natadd (my_natmul n1 n2 (SUCC x)) n2;;

let natmul : nat -> nat -> nat
= fun n1 n2 -> my_natmul n1 n2 ZERO;;

let two =SUCC (SUCC ZERO);;
let three = SUCC (SUCC (SUCC ZERO));;

natmul three two;;
natmul two three;;
natadd three two;;
natadd two three;;