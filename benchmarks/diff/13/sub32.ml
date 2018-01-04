type ae = CONST of int
| VAR of string
| POWER of string * int
| TIMES of ae list
| SUM of ae list

exception InvalidArgument

let rec diff (expr, variable) = 
	match expr with 
		CONST(i) -> CONST(0)
		| VAR(str) -> (
			if (variable = str) then CONST(1)
			else CONST(0))
		| POWER(str, i) ->(
			if (variable = str) then (TIMES((CONST(i))::(POWER(str, i-1))::[]))
			else CONST(0))
		| TIMES(l) -> (
			match l with
				[] -> raise InvalidArgument
				| hd::[] -> (diff (hd, variable))
				| hd::tl -> (SUM((TIMES((diff (hd, variable))::tl))::(TIMES(hd::(diff ((TIMES(tl)), variable))::[]))::[]))) 
		| SUM(l) ->(
			match l with
				[] -> raise InvalidArgument
				| hd::[] -> (diff (hd, variable))
				| hd::tl -> (SUM((diff (hd, variable))::(diff ((SUM(tl)), variable))::[])))

