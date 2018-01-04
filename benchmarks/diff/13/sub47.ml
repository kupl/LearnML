type ae = CONST of int
		| VAR of string
		| POWER of string * int
		| TIMES of ae list
		| SUM of ae list

exception InvalidArgument

let rec diff (ae, instr) =
	match ae with
	| CONST a -> CONST 0
	| VAR x -> if x=instr then CONST 1
				else CONST 0
	| POWER (x, y) -> if x=instr && y=0 then CONST 0
						else if x=instr && y<>0 then TIMES [CONST y;POWER(x, y-1)]
						else CONST 0
	| TIMES [] -> raise InvalidArgument
	| TIMES lst -> timeProcess(lst, instr)
	| SUM [] -> raise InvalidArgument
	| SUM lst -> SUM (List.map (fun x -> diff(x, instr)) lst)

and timeProcess (l, istr) =
	let check x =
	match x with
	| VAR x -> if x=istr then true
				else false
	| POWER(x, _) -> if x=istr then true
					else false
	| _ -> false in
	
	let varList = List.filter (fun x -> check x) l in
	
	if (List.exists (fun x -> check x) l) then
		TIMES (diff(polyProcess(varList, istr),istr) :: (List.filter (fun x -> (not (check x))) l))
	
	else
		CONST 0

and polyProcess (l, is) =
	let varToPower = function
					| VAR x -> POWER(x, 1)
					| POWER (x, y) -> POWER (x, y)
					| _ -> raise InvalidArgument in
	let powerFold x = function
					| POWER(_, a) -> x+a
					| _ -> raise InvalidArgument in
	let poweredList = List.map (fun x -> varToPower(x)) l in
	let rec fold_left f a l =
		match l with
		| [] -> a
		| h::t -> fold_left f (f a h) t in
	let muled = fold_left powerFold 0 poweredList in
	POWER(is, muled)