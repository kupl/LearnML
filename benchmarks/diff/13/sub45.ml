type ae = CONST of int
	| VAR of string
	| POWER of string * int
	| TIMES of ae list
	| SUM of ae list

exception InvalidArgument

let rec diff (eq, var) =
	match eq with
		| CONST n 	-> CONST 0
		| VAR x		-> if x = var then CONST 1
				   else CONST 0
		| POWER (x, n)	-> if x = var then TIMES [CONST n; POWER (x, n-1)]
				   else CONST 0
		| TIMES lst 	->
			(match lst with
				| [] 		-> raise InvalidArgument
				| head :: []	-> diff (head, var)
				| head :: tail 	-> SUM [(TIMES ((diff(head, var)) :: tail)); (TIMES [head; (diff (TIMES tail, var))])])
		| SUM lst	->
			(match lst with
				| []		-> raise InvalidArgument
				| head :: []	-> diff (head, var)
				| head :: tail  -> SUM [(diff(head, var)); (diff(SUM tail, var))])
