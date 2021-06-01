type nat = ZERO | SUCC of nat


let rec natadd n1 n2 (* nat -> nat -> nat *)
= (*fun two three -> *) 
match n1 with
  | ZERO -> n2
  | SUCC n1_1 -> natadd n1_1 (SUCC n2)

let rec natmul n1 n2 =
  match n1 with
    | ZERO -> ZERO
    | SUCC n1_1 -> natadd n2 (natmul n1_1 n2)  ;;


