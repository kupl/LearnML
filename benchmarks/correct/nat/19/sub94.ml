type nat = ZERO | SUCC of nat

let rec length n = 
  match n with
    | ZERO -> 0
    | SUCC (n') -> 1 + length n';;

let rec result sum = 
    match sum with
      | 0 -> ZERO
      | _ -> SUCC (result (sum-1));;

let natadd : nat -> nat -> nat
= fun n1 n2 -> result (length n1 + length n2);;

let natmul : nat -> nat -> nat
= fun n1 n2 -> result (length n1 * length n2);;

let three = SUCC (SUCC (SUCC ZERO));;
let two = SUCC (SUCC ZERO);;
let zero = ZERO;;
let one = SUCC ZERO;;
natadd two three;;
natmul two three;;
natadd zero one;;
natmul zero one;;