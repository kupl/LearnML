type nat = ZERO | SUCC of nat

let rec natadd : nat * nat -> nat = fun (n1, n2) ->
	match (n1, n2) with
	| ( ZERO, _ ) ->
		n2
	| ( _ , ZERO ) ->
		n1
	| ( SUCC n1' , SUCC n2' ) ->
		natadd (n1', SUCC n2)

let natmul : nat * nat -> nat = fun (n1, n2) ->
	match (n1, n2) with
	| ( ZERO, _ ) ->
		ZERO
	| ( _ , ZERO ) ->
		ZERO
	| ( SUCC n1' , SUCC n2' ) ->
		let rec iteradd : nat * nat * nat -> nat = fun (na1, na2, result) ->
			match (na1, na2) with
			| (ZERO, _) ->
				result
			| (_, ZERO) ->
				result
			| (SUCC na1', SUCC na2') ->
				iteradd (na1', na2, natadd(na2, result))
		in
			iteradd (n1, n2, ZERO)