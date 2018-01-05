(* Exercise 6 *)
type nat =
	ZERO
	| SUCC of nat

let rec natadd (a, b) =
	match (a, b) with
		(ZERO, ZERO) ->
			ZERO
		| (_, ZERO) ->
			a
		| (ZERO, _) ->
			b
		| (SUCC n, _) ->
			(SUCC (natadd (n, b)))

let rec natmul (a, b) =
	match (a, b) with
		(_, ZERO) ->
			ZERO
		| (ZERO, _) ->
			ZERO
		| (_, SUCC (ZERO)) ->
			a
		| (SUCC (ZERO), _) ->
			b
		| (SUCC n, _) ->
			natadd (b, (natmul (n, b)))

(* Function 'int_of_nat' is for debug *)
let rec int_of_nat n =
	match n with
		ZERO ->
			0
		| SUCC a ->
			(1 + (int_of_nat a))
