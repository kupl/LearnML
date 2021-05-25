type nat =
	| ZERO
	| SUCC of nat

let addition a b  
= SUCC(b);;
    
let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
 match n1 with
 |ZERO -> n2 
 |SUCC(nat) -> natadd nat (addition nat n2) ;; 

let rec natmul : nat -> nat -> nat
=fun n1 n2  ->
 match n1 with
 |ZERO -> ZERO
 |SUCC(ZERO) -> n2
 |SUCC(nat) -> natadd n2 (natmul nat n2) ;; 
