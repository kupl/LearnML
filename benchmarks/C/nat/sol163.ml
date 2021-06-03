type nat =
	| ZERO
	| SUCC of nat

let rec natadd a b =
match b with
|ZERO -> a
|SUCC n -> natadd (SUCC a) n 

let natdec a = 
match a with
| ZERO -> raise (Failure "Cannot go below ZERO")
| SUCC nat -> nat

let rec nataddntimes a n b =
match n with
|ZERO -> ZERO
|SUCC ZERO -> a
|SUCC nat -> nataddntimes (natadd a b) (natdec n) b

let rec natmul a b =
nataddntimes a b a
