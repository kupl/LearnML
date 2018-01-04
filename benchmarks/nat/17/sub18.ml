type nat = ZERO | SUCC of nat 

let rec natadd (n1:nat) (n2:nat) = 
  match n1 with
  |ZERO -> n2
  |SUCC(pred1) -> 
    match n2 with
    |ZERO -> n1
    |SUCC(pred2) -> natadd pred1 (SUCC n2) 


let rec natmul (n1:nat) (n2:nat) = 
  match n1 with
  |ZERO -> ZERO
  |SUCC ZERO -> n2
  |SUCC(pred1) -> 
    match n2 with
    |ZERO -> ZERO
    |SUCC ZERO -> n1
    |SUCC(pred2) -> natadd n2 (natmul pred1 n2)

