exception InvalidArgument

type ae =
	| CONST of int
	| VAR of string
	| POWER of string * int
	| TIMES of ae list
	| SUM of ae list

let rec isVarExist (exp, var) =
	match exp with
	| CONST n -> false
	| VAR str -> if str = var then true else false
	| POWER (str, n) -> if str = var then true else false
	| TIMES (head :: tail) ->
			(isVarExist (head, var)) || (isVarExist ((TIMES tail), var))
	| SUM (head :: tail) ->
			(isVarExist (head, var)) || (isVarExist ((SUM tail), var))
	| _ -> false

let rec countVar (l, var) =
	match l with
	| [] -> 0
	| head :: tail ->
			if isVarExist (head, var)
			then 1 + (countVar (tail, var))
			else countVar (tail, var)

let flag = ref 0

let rec diff (exp, var) =
	match exp with
	| CONST x -> CONST 0
	| VAR str -> if str = var then CONST 1 else CONST 0
	| POWER (str, n) ->
			if str = var
			then TIMES [ CONST n; POWER (str, (n - 1)) ]
			else CONST 0
	| TIMES [] -> raise InvalidArgument
	| TIMES (head :: tail) ->
			if isVarExist (head, var)
			then
				if (countVar (tail, var)) = (List.length tail)
				then
					let rec multDiff =
						function
						| ([ head; tail ], var) ->
								if (List.length [ head; tail ]) > !flag
								then
									(flag := !flag + 1;
										(TIMES [ diff (head, var); tail ]) ::
										(multDiff ([ tail; head ], var)))
								else (CONST 1)::[]
						| _ -> raise InvalidArgument
					in SUM (multDiff ((head :: tail), var))
				else diff ((TIMES (List.rev (head :: List.rev tail)), var))
			else if (List.length tail) = 1 then TIMES (head :: diff (List.hd tail, var) ::[])
			else TIMES (head :: diff (TIMES tail, var) ::[])
	| SUM [] -> raise InvalidArgument
	| SUM (head :: tail) ->
			if (List.length tail) = 1 then SUM ((diff (head, var)) :: (diff (List.hd tail, var))::[])
			else SUM ((diff (head, var)) :: (diff (SUM tail, var))::[])