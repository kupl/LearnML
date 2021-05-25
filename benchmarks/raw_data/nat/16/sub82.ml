type nat =
	| ZERO
	| SUCC of nat

let rec getPrevious : nat -> nat -> nat
= fun n1 n2 -> 
if (SUCC(n1) = n2) then n1 
else getPrevious (SUCC(n1)) n2 


let rec natadd : nat -> nat -> nat
= fun n1 n2 ->	
if n2 = ZERO then  n1 
else natadd (SUCC(n1))  (getPrevious ZERO n2)

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
if n2 = ZERO then ZERO 
else if n1 = ZERO then ZERO 
else if n1 = SUCC(ZERO) then n2  
else if n2 = SUCC(ZERO) then n1 
else natadd n1 (natmul n1 (getPrevious ZERO n2))
