type nat = ZERO | SUCC of nat

let natadd : nat -> nat -> nat
= fun n1 n2 -> let rec nat1 = fun x y ->
  if x <> n1 then nat1 (SUCC x) (SUCC y) else y in nat1 ZERO n2;; 

let natmul : nat -> nat -> nat
= fun n1 n2 -> let rec nat1 = fun x y ->
  if y <> n2 then nat1(natadd x n1) (SUCC y) else x in nat1 ZERO ZERO;;
  
  let two = SUCC (SUCC ZERO);;
  let three = SUCC (SUCC (SUCC ZERO));;
  natmul two three;;
  natadd two three;;
