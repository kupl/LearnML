(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp;;

let calculator : exp -> int
= fun e -> let rec loop e xAssigned xValue =
	match e with
	| X -> if xAssigned = false
			then raise (Failure "XnotAssignedError: var X is only able to be used in SIGMA")
		else xValue

	| INT n -> n

	| ADD(e1, e2) -> 
		((loop e1 xAssigned xValue) + (loop e2 xAssigned xValue))

	| SUB(e1, e2) -> 
		((loop e1 xAssigned xValue) - (loop e2 xAssigned xValue))

	| MUL(e1, e2) ->
		((loop e1 xAssigned xValue) * (loop e2 xAssigned xValue))

	| DIV(e1, e2) -> 
		let n2 = loop e2 xAssigned xValue in 
		if n2 = 0 then raise (Failure "DivideByZeroError: impossible to divide an integer by zero")
		else ((loop e1 xAssigned xValue) / n2)

	| SIGMA(e1, e2, f) -> 	
		let n1 = loop e1 xAssigned xValue in
		let n2 = loop e2 xAssigned xValue in
		let sum = 0 in
		if n1 > n2 then raise (Failure "SIGMAformError: n1 should be less or equal then n2")
		else let rec cal_sigma n = 
			if n > n2 then sum
			else (sum + (loop f true n) + (cal_sigma (n+1))) 
		in cal_sigma n1

	in (loop e false 0)