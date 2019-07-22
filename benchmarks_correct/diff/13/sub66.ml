type ae = CONST of int
		| VAR of string
		| POWER of string * int
		| TIMES of ae list
		| SUM of ae list

exception InvalidArgument

let rec diff (e, str) =
	match e with
	| CONST _ -> CONST 0
	| VAR s ->
		if s = str then CONST 1
		else CONST 0
	| POWER (s, n) ->
		if s = str then TIMES([CONST n; POWER(s, n-1)])
		else CONST 0
	| TIMES aelist ->
		if aelist = [] then raise InvalidArgument
		else if List.length aelist = 1 then diff (List.hd aelist, str)
		else SUM (TIMES (diff (List.hd aelist, str) :: List.tl aelist) :: TIMES (List.hd aelist :: diff (TIMES (List.tl aelist), str) :: []) :: [])
	| SUM aelist ->
		if aelist = [] then raise InvalidArgument
		else if List.length aelist = 1 then diff (List.hd aelist, str)
		else SUM (diff (List.hd aelist, str) :: diff (SUM (List.tl aelist), str) :: [])
