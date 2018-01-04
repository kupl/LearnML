exception InvalidArgument

type ae = CONST of int
| VAR of string
| POWER of string * int
| TIMES of ae list
| SUM of ae list

let rec r_diff (ae, str) =
	match ae with
	| CONST i -> CONST 0
	| VAR v -> 
		if v = str then CONST 1
		else CONST 0
	| POWER (v, i) ->
		if (not (v = str)) then CONST 0
		else if i = 0 then CONST 0
		else if i = 1 then CONST 1
		else if i = 2 then TIMES [CONST i;VAR v]
		else TIMES [CONST i;POWER (v, i-1)]
	| TIMES (hd::tl) ->
		if tl = [] then r_diff (hd, str)
		else SUM [(TIMES ((r_diff (hd, str))::tl));(TIMES [hd;(r_diff (TIMES tl, str))])]
	| TIMES [] -> raise InvalidArgument
	| SUM (hd::tl) ->
		if tl = [] then r_diff (hd, str)
		else SUM [r_diff(hd, str);r_diff(SUM tl, str)]
	| SUM [] -> raise InvalidArgument

let rec contain_zero lst =
	match lst with
	| [] -> false
	| hd::tl ->
		if hd = CONST 0 then true
		else contain_zero tl

let rec minimize_sum lst =
	match lst with
	| [] -> []
	| hd::tl ->
		match hd with
		| CONST 0 -> minimize_sum tl
		| SUM lst -> lst@(minimize_sum tl)
		| _ -> hd::(minimize_sum tl)

let rec minimize_times lst =
	match lst with
	| [] -> []
	| hd::tl ->
		match hd with
		| CONST 1 -> minimize_times tl
		| TIMES lst -> lst@(minimize_times tl)
		| _ -> hd::(minimize_times tl)



let rec minimize ae = 
	match ae with
	| CONST i -> CONST i
	| VAR v -> VAR v
	| POWER (v, i) -> POWER (v, i) 
	| SUM lst ->
		if (lst = []) then CONST 0
		else if (List.length lst) = 1 then List.hd lst
		else SUM (List.map minimize (minimize_sum lst))
	| TIMES lst ->
		if (contain_zero lst) then CONST 0
		else if (List.length lst) = 1 then List.hd lst
		else TIMES (List.map minimize (minimize_times lst))

let rec r_minimize ae = 
	if ae = (minimize ae) then ae
	else r_minimize (minimize ae)

let diff (ae, str) = r_minimize (r_diff (ae, str))
