type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> ZERO 
match n1 with 

	|ZERO -> n2

	|SUCC(a)-> natadd a (SUCC(n2))

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> ZERO
if n2=ZERO then ZERO

  else if n2=SUCC(ZERO) then n1

  else 

   match n1 with 

   |ZERO -> ZERO

   |SUCC(ZERO)-> n2 

   |SUCC(a) -> natadd (natmul a n2) n2
