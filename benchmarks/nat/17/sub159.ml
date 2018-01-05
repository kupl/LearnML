type nat = ZERO
         | SUCC of nat

let rec natadd ( (nat1 : nat), (nat2 : nat) ) =
  match nat1 with
    ZERO -> nat2
  | SUCC n -> natadd (n,(SUCC nat2))
  
let rec natmul ( (nat1 : nat), (nat2 : nat) ) = 
  match nat1 with
    ZERO -> ZERO
  | SUCC n -> natadd( nat2, natmul(n, nat2) )