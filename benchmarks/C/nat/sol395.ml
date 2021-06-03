type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n2  with
                ZERO -> n1
              | SUCC a -> SUCC (natadd n1 a);;


let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n2 with
                ZERO -> ZERO
              | SUCC a -> natadd (natmul n1 a) n1;;
  
  

let two = SUCC (SUCC ZERO);;
let three = SUCC (SUCC (SUCC ZERO));;

natmul two three;;
natadd two three;;