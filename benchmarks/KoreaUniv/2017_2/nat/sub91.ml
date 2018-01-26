(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 ->
match n1 with
| ZERO -> n2
| SUCC n1 -> SUCC(natadd n1 n2)

let rec natmul : nat -> nat -> nat 
= fun n1 n2 -> 
match n1 with
| ZERO -> ZERO
| SUCC n1 -> natadd n2 (natmul n1 n2)

let rec natexp : nat -> nat -> nat 
= fun n1 n2 -> 
match n1 with
| ZERO -> raise (Failure ("Invalid_argument"))
| SUCC ZERO -> SUCC ZERO
| _ ->
        match n2 with
        | ZERO -> SUCC ZERO
        | SUCC ZERO -> n1
        | SUCC n2 -> natmul n1 (natexp n1 n2)
