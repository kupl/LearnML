type nat = ZERO | SUCC of nat

let rec evaluate = fun n ->
  match n with
    | ZERO -> 0
    | SUCC (k) -> 1 + evaluate k;;
    
let rec construct = fun n ->
  if n ==0 then ZERO
  else SUCC(construct (n - 1) );;
  
let natadd : nat -> nat -> nat
= fun n1 n2 -> construct(evaluate n1 + evaluate n2);;

let natmul : nat -> nat -> nat
= fun n1 n2 -> construct(evaluate n1 * evaluate n2);;

(*let two = SUCC (SUCC ZERO);;*)
(*let three = SUCC (SUCC (SUCC ZERO));;*)
(*natmul two three;;*)
(*natadd two three;;*)
