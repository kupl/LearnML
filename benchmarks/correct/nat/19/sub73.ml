type nat = ZERO | SUCC of nat

let natadd : nat -> nat -> nat
= fun n1 n2 -> (*TODO*)
  let rec innatadd x y = 
    match y with 
      | ZERO -> x
      | SUCC a -> innatadd (SUCC x) a
  in innatadd n1 n2;;

let natmul : nat -> nat -> nat
= fun n1 n2 -> (*TODO*)
  let rec innatmul x y z=
    match y with
      | ZERO -> z
      | SUCC a -> innatmul x a (natadd z x)
  in innatmul n1 n2 ZERO;;

let two = SUCC (SUCC ZERO);;
let three = SUCC (SUCC (SUCC ZERO));;

natadd two three;;
natmul two three;;
