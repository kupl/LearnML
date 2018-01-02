type nat =
   | ZERO
   | SUCC of nat
 
let rec natadd : nat -> nat -> nat
   = fun n1 n2 ->
       match n1 with
       | ZERO -> n2
       | SUCC n1 -> SUCC(natadd n1 n2)
 
let rec f : nat -> nat -> nat
   = fun n1 n2 ->
     match n1 with
     | ZERO -> n2
     | SUCC n1 -> SUCC(f n1 (f n1 n2))   
