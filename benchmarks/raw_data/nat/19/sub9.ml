type nat = ZERO | SUCC of nat

let zero = ZERO;;

let one = SUCC ZERO;;

let two = SUCC one;;

let three = SUCC two;;


let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
  match n1 with
    | ZERO -> n2
    | SUCC n1 -> SUCC(natadd n1 n2)
;;


let rec makeLess : nat -> nat -> nat (*we always want to plug in 0 because it is where we start*)
= fun newNum prevNum ->
  if SUCC newNum = prevNum then newNum
  else makeLess (SUCC newNum) prevNum
;;

let rec natmul : nat -> nat -> nat
  = fun n1 n2 -> 
  match n2 with
    | ZERO -> ZERO
    |_ -> natadd n1 (natmul n1 (makeLess ZERO n2))
;;

natmul three two;;
