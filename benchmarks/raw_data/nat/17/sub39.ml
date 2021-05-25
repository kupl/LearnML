(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> 
  if n1 = ZERO then n2
  else match n2 with 
        | ZERO -> n1
        | SUCC a -> natadd (SUCC n1) a

let rec natmul : nat -> nat -> nat 
= fun n1 n2 -> (* TODO *)
  if n1 = ZERO || n2 = ZERO then ZERO
else if n1 = SUCC ZERO then n2
else if n2 = SUCC ZERO then n1
else match n1 with 
	| ZERO -> ZERO
	| SUCC a -> natadd (natmul a n2) n2

let rec natexp : nat -> nat -> nat 
= fun n1 n2 -> (* TODO *)
  match n2 with
  | ZERO -> SUCC ZERO
  | SUCC a -> if a = ZERO then n1
              else natmul n1 (natexp n1 a)
