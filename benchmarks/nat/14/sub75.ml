
type nat = ZERO | SUCC of nat

let rec natadd : nat * nat -> nat = fun (a,b) ->
	let arg1 = fst(a,b) in
	match arg1 with
	| ZERO -> snd(a,b)
	| SUCC n -> SUCC(natadd(n,snd(a,b))) 

let rec natmul : nat * nat -> nat = fun (a,b) ->
	let arg1 = fst(a,b) in
	match arg1 with
	| ZERO -> ZERO
	| SUCC n -> natadd(natmul(n,snd(a,b)),snd(a,b)) 

