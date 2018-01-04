(* 
2011-10634
JooHyun Jo / Major in Economics
problem 4 for HW1
*)

type nat = ZERO | SUCC of nat

let rec natadd ((n1:nat), (n2:nat)) :nat =
  match n1 with
  |ZERO -> n2
  |SUCC(prev1) ->(
    match n2 with
    |ZERO -> n1
    |SUCC(prev2)-> natadd (prev1, (SUCC n2))
    ) 

let rec natmul ((n1:nat), (n2:nat)):nat =
  match n1 with
  |ZERO -> ZERO
  |SUCC(prev1) ->(
    match n2 with
    |ZERO -> ZERO
    |SUCC(prev2) -> natadd (n2, (natmul (prev1, n2)))
  )

