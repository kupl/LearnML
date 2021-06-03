type nat = ZERO | SUCC of nat

let rec natadd n1 n2 =
  match n2 with
    |ZERO -> n1
    |SUCC ZERO -> SUCC n1
    |SUCC (a)-> SUCC(natadd n1 a);;


let rec natmul n1 n2 = 
  if n1 = ZERO then ZERO
  else
    match n2 with 
      |ZERO -> ZERO
      |SUCC ZERO -> n1
      |SUCC (a) -> natadd (natmul n1 a) n1 ;;
