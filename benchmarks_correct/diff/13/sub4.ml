(* KIHWAN KANG HW02-2 *)

(* PREDEFINED TYPES *)
type ae = 
	CONST of int
	|VAR of string
	|POWER of string * int
	|TIMES of ae list
	|SUM of ae list

exception InvalidArgument
(* END OF PREDEFINED TYPES *)

let rec diff (expr, var_ref) =
	let lowerPow (var, pow) = 
	(
		if pow = 1 
		then (CONST 1)
		else (TIMES [(CONST pow);(POWER (var, pow - 1))])
	)
in 
	let rec diffSum (aelist, var_ref) =
	(
		match aelist with
		|[] -> []
		|head::tail -> (diff (head, var_ref))::(diffSum (tail, var_ref))
	)
in 
	let rec diffTim (prelist, aelist, var_ref) =
	(
		match aelist with
		|[] -> []
		|head::tail -> 
			(TIMES (prelist@[diff (head, var_ref)]@tail))::
			(diffTim (prelist@[head], tail, var_ref))
	)
in
	match expr with
	|CONST _ -> (CONST 0)
	|VAR var -> 
		if var = var_ref 
		then (CONST 1)
		else (CONST 0)
	|POWER (var, pow) ->
		if var = var_ref & pow != 0
		then lowerPow (var, pow)
		else (CONST 0)
	|TIMES aelist -> 
		(
		match aelist with
			|[] -> raise InvalidArgument
			|head::tail -> (SUM (diffTim ([], aelist, var_ref)))
		)
	|SUM aelist ->
		(
		match aelist with
			|[] -> raise InvalidArgument 
			|head::tail -> (SUM (diffSum (aelist, var_ref)))
		)
