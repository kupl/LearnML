type nat = ZERO | SUCC of nat;;

let rec nat_to_int x =
  match x with 
    |ZERO -> 0
    |SUCC e1-> 1 + nat_to_int(e1);;
    
     

let rec int_to_nat x =
  match x with 
    |0 -> ZERO
    |_ -> SUCC  (int_to_nat (x-1));;
    


    
let natadd : nat -> nat -> nat
= fun n1 n2 -> 
int_to_nat ((nat_to_int n1) + (nat_to_int n2));;


let natmul : nat -> nat -> nat
= fun n1 n2 -> 
int_to_nat ((nat_to_int n1) * (nat_to_int n2));;


(*test*)
nat_to_int (natmul (SUCC(SUCC ZERO)) (SUCC(SUCC(SUCC ZERO))));;
