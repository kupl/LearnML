(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 ->
  match n1 with
  | ZERO -> n2
  | SUCC n3 -> SUCC (natadd n3 n2)

let rec natmul : nat -> nat -> nat 
= fun n1 n2 ->
  if n1 = ZERO then ZERO
  else match n2 with
  | ZERO -> ZERO
  | SUCC n3 -> natadd (n1) (natmul n1 n3)

let rec natexp : nat -> nat -> nat 
= fun n1 n2 ->
  if n2 = ZERO then SUCC ZERO
  else match n2 with
  | SUCC n3 -> natmul (n1) (natexp n1 n3)
