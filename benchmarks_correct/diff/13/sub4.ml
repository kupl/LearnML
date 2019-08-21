(* KIHWAN KANG HW02-2 *)

(* PREDEFINED TYPES *)
type aexp = 
	Const of int
	|Var of string
	|Power of string * int
	|Times of aexp list
	|Sum of aexp list

exception InvalidArgument
(* END OF PREDEFINED TYPES *)

let rec diff (expr, var_ref) =
	let lowerPow (var, pow) = 
	(
		if pow = 1 
		then (Const 1)
		else (Times [(Const pow);(Power (var, pow - 1))])
	)
in 
	let rec diffSum (aexplist, var_ref) =
	(
		match aexplist with
		|[] -> []
		|head::tail -> (diff (head, var_ref))::(diffSum (tail, var_ref))
	)
in 
	let rec diffTim (prelist, aexplist, var_ref) =
	(
		match aexplist with
		|[] -> []
		|head::tail -> 
			(Times (prelist@[diff (head, var_ref)]@tail))::
			(diffTim (prelist@[head], tail, var_ref))
	)
in
	match expr with
	|Const _ -> (Const 0)
	|Var var -> 
		if var = var_ref 
		then (Const 1)
		else (Const 0)
	|Power (var, pow) ->
		if var = var_ref & pow != 0
		then lowerPow (var, pow)
		else (Const 0)
	|Times aexplist -> 
		(
		match aexplist with
			|[] -> raise InvalidArgument
			|head::tail -> (Sum (diffTim ([], aexplist, var_ref)))
		)
	|Sum aexplist ->
		(
		match aexplist with
			|[] -> raise InvalidArgument 
			|head::tail -> (Sum (diffSum (aexplist, var_ref)))
		)
