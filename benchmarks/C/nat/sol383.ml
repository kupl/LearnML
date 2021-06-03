type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
  match n1 with
    | ZERO -> n2
(*    | SUCC ZERO -> SUCC n2*)
    | SUCC m -> natadd m (SUCC n2)

let rec natmul : nat -> nat -> nat
  = fun n1 n2 ->
    match n2 with
    | ZERO -> ZERO
    (*| SUCC ZERO -> n1*)
    | SUCC m -> natadd (natmul n1 m) n1;;
    
let one = SUCC ZERO;;
let two = SUCC (SUCC ZERO);;
let three = SUCC(two);;
