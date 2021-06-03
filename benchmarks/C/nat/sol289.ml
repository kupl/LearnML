type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->  
  match n1 with
  ZERO -> n2
  |_ ->  (match n2 with
    ZERO -> n1
    |SUCC(m) -> let ad = SUCC(n1) in natadd ad m);;


let rec natmul : nat -> nat -> nat
= fun n1 n2 ->    
  match n1 with
  ZERO -> ZERO
  |_ -> (match n2 with
    ZERO -> ZERO
    | SUCC(m) -> let mu = n1 in natadd mu (natmul n1 m));;

let two = SUCC (SUCC ZERO);;
let three = SUCC (SUCC (SUCC ZERO));;
natmul two three;;
natadd two three;;