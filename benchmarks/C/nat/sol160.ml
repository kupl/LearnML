type nat =
    | ZERO
    | SUCC of nat
  
  let rec natadd : nat -> nat -> nat
    = fun n1 n2 -> match n2 with
  |ZERO -> n1
  |SUCC n -> natadd (SUCC n1) n
  
  let rec natmul : nat -> nat -> nat
  = fun n1 n2 -> if n1 = ZERO || n2 = ZERO then ZERO else match n2 with 
  |ZERO -> ZERO 
  |SUCC(ZERO) -> n1
  |SUCC(n) -> natadd n1 (natmul n1 n)