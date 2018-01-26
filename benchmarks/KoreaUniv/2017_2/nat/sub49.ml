
(* problem 2*)
let rec natadd : nat -> nat -> nat = fun a b -> if a != ZERO then
  (match a with
    | ZERO -> ZERO
    | SUCC t -> SUCC (natadd t b))
  else 
  (match b with 
    | ZERO -> ZERO
    | SUCC t -> SUCC (natadd ZERO t));;

let rec natmul : nat -> nat -> nat = fun a b ->
  match a with
    | ZERO -> ZERO
    | SUCC t -> natadd (natmul t b) b;;

let rec natexp : nat -> nat -> nat = fun a b ->
  match b with
    | ZERO -> SUCC ZERO
    | SUCC t -> natmul a (natexp a t);;