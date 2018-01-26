(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat = fun n1 n2 ->
match n1 with
|ZERO -> n2
|SUCC(n) -> SUCC(natadd n n2);;

let rec natmul : nat -> nat -> nat = fun n1 n2 ->
match n1 with
|ZERO -> ZERO
|SUCC(ZERO) -> n2
|SUCC(n) -> natadd (natmul (SUCC(ZERO)) n2) (natmul n n2);;

let rec natexp : nat -> nat -> nat = fun n1 n2 ->
match n1 with
|ZERO -> ZERO
|SUCC(ZERO) -> SUCC ZERO
|SUCC(n) -> match n2 with |ZERO -> SUCC ZERO |SUCC(ZERO) -> n1 |SUCC(m) ->
natmul n1 (natexp n1 m);;
