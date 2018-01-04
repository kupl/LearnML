type nat = ZERO | SUCC of nat

let 
  let rec natadd ((nat1 : nat), (nat2 : nat)) : nat =
    match nat1 with
      |ZERO -> nat2
      |SUCC (nat1)-> natadd(nat1-1, nat2+1)

let rec natmul ((nat1 : nat), (nat2 : nat)) : nat =
  match nat1 with
    |ZERO -> 0
    |SUCC (nat1)-> iter(nat1, nat2) in 
let rec iter((n1 : nat), (n2 : nat)) : nat =
  match n with
    |ZERO -> 0
    |SUCC (nat1)-> iter(n1-1, natadd(n1, n2))

