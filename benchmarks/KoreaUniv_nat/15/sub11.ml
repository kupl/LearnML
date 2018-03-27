type nat = ZERO | SUCC of nat;;

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> 
  let before (SUCC c) = if (SUCC c)=ZERO then ZERO else c in
  if n1 = ZERO then n2
  else SUCC(natadd (before n1) n2);;


let rec natmul : nat -> nat -> nat
=fun n1 n2 ->
  let before (SUCC c) = if (SUCC c)=ZERO then ZERO else c in
  if n1 = ZERO then ZERO
  else natadd (natmul (before n1) n2) n2;;

(* ¡°before a¡± function returns the number before a(opposite of SUCC) *)
(* used recursive definition that ((a-1)+b)+1=(a+b) and ((a-1)*b)+b=(a*b) *)
