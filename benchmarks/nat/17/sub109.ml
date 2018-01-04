type nat = ZERO | SUCC of nat

let pred : nat -> nat = fun n ->
	match n with
	| ZERO -> raise (Failure "invalid")
	| SUCC(m) -> m

let rec natadd : (nat * nat) -> nat = fun (n,m) ->
	match n with
	| ZERO -> m
	| SUCC (n1) -> natadd (n1, SUCC(m))

let rec naticr : (nat * nat * nat) -> nat = fun (n,l,m) ->
	match n with
	| ZERO -> l
	| SUCC (n1) -> naticr (n1, natadd(l,m), m)

let natmul : (nat * nat) -> nat = fun (n,m) ->
	match n with
	| ZERO -> ZERO
	| SUCC (n1) -> naticr (n, ZERO, m)
