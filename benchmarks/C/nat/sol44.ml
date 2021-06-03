type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> 
  match (n1, n2) with
  | (ZERO, ZERO) -> ZERO
  | (ZERO, SUCC nat1) -> SUCC (natadd ZERO nat1)
  | (SUCC nat1, ZERO) -> SUCC (natadd nat1 ZERO)
  | (SUCC nat1, SUCC nat2) -> SUCC(SUCC(natadd nat1 nat2))  

let rec natmul : nat -> nat -> nat
=fun n1 n2 ->
  let rec count : nat -> int
  =fun n ->
    match n with
    | ZERO -> 0
    | SUCC a -> 1 + count a 
  in 
  let rec natinit : int -> nat
  =fun i ->
    if i > 0 then SUCC (natinit (i-1))
    else ZERO 
  in
  natinit ((count n1) * (count n2))
