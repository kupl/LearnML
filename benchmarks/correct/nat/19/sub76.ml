type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
  match n2 with
    | ZERO -> 
        let rec evalnat a = 
          match a with
          | ZERO -> ZERO
          | SUCC (b) -> SUCC (evalnat b)
        in
        evalnat n1
    | SUCC (b2) -> SUCC ( natadd n1 b2 );;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
   match n2 with
    | ZERO -> ZERO
    | SUCC (b) -> natadd n1 (natmul n1 b);;
    
let two = SUCC (SUCC ZERO);;
let three = SUCC (SUCC (SUCC ZERO));;

natadd two three;;
natmul two three;;


    

