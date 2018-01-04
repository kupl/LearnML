exception Error of string
type nat = ZERO | SUCC of nat
(*
let rec makenat : int -> nat =
	(function a ->
		if (a <0) then raise (Error "Invalid error.")
			else if (a = 0) then ZERO
				else (SUCC (makenat (a-1))))
let rec evalnat : nat -> int =
	(function a ->
		(match a with
			ZERO -> 0
			| SUCC b -> ((evalnat b) + 1)))
*)
let rec natadd : nat * nat -> nat =
	(function (n1,n2) ->
		(match (n1,n2) with
			(ZERO,_) -> n2
			|(_,ZERO) -> n1
			|(SUCC a1, SUCC a2) -> (natadd (a1, (SUCC n2)))))
let rec natmul : nat * nat -> nat =
	(function (n1,n2) ->
		(match (n1,n2) with
			(ZERO,_) -> ZERO
			|(_,ZERO) -> ZERO
			|(SUCC a1, SUCC a2) -> (natadd (n2, (natmul (a1, n2))))))

