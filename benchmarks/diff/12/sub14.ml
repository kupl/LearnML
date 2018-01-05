type ae = CONST of int
		| VAR of string
		| POWER of string * int
		| TIMES of ae list
		| SUM of ae list
		
let rec diff (ae, str) =
	let rec isThereStr ae str =
		match ae with
		CONST i -> false
		| VAR s -> (s = str)
		| POWER (s, i) -> (s = str)
		| TIMES l -> (match l with
						[] -> false
						| h::t -> (isThereStr h str) || (isThereStr (TIMES t) str))
		| SUM l -> (match l with
					[] -> false
					| h::t -> (isThereStr h str) || (isThereStr (SUM t) str))
	in
	let rec mul al str =
		match al with
		[] ->  []
		| h::[] -> if(isThereStr h str) then [diff(h, str)] else [h]
		| h::t -> if(isThereStr h str) then List.append [diff(h, str)] (mul t str) else List.append [h] (mul t str)
	in
	match ae with
	CONST i -> CONST 0
	| VAR s -> if (s = str) then CONST 1
				else CONST 0
	| POWER (s, i) -> if (s = str) then ( if (i > 1) then TIMES [(CONST i); (POWER (s, i-1))]
											else CONST 1 )
						else CONST 0
	| TIMES l -> if (isThereStr ae str) then TIMES (mul l str)
					else CONST 0
	| SUM l -> match l with
				[] -> CONST 0
				| h::[] -> diff (h, str)
				| h::t -> SUM (List.append [diff (h, str)] [diff ( (SUM t), str ) ])