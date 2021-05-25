
(*problem 2*)
type nat = ZERO | SUCC of nat

let natadd : nat -> nat -> nat
= fun n1 n2 ->
let rec naadd n1 n2 = 
match n1 with
| ZERO -> n2 | SUCC n1 -> SUCC (naadd n1 n2) in naadd n1 n2;;

let natmul : nat -> nat -> nat
= fun n1 n2 ->
let rec namul n1 n2 =
match n2 with
| ZERO -> ZERO
| SUCC a -> natadd n1 (namul n1 a) in namul n1 n2;;

let natexp : nat -> nat -> nat
= fun n1 n2 ->
let rec naexp n1 n2 =
match n2 with 
| ZERO -> (SUCC ZERO)
| SUCC a -> natmul n1 (naexp n1 a) in naexp n1 n2;;