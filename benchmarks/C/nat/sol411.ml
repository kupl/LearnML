type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
  match n1 with
    | ZERO -> n2
    | SUCC(arg) -> natadd arg (SUCC n2);;

let rec natmul_in : nat -> nat -> nat -> nat
= fun n_t n_a n_r -> 
  match n_t with
    | ZERO -> ZERO
    | SUCC ZERO -> n_r
    | SUCC(arg) -> natmul_in arg n_a (natadd n_r n_a);;

let natmul : nat -> nat -> nat
= fun n1 n2 -> 
  match n1 with
    | ZERO -> ZERO
    | SUCC ZERO -> n2
    | SUCC(arg) -> natmul_in n1 n2 n2;;
    
let two = SUCC(SUCC ZERO);;
let three = SUCC(SUCC (SUCC ZERO));;
let four = SUCC(SUCC(SUCC (SUCC ZERO)));;