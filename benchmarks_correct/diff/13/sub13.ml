type ae = CONST of int
		| VAR of string
		| POWER of string * int
		| TIMES of ae list
		| SUM of ae list

exception InvalidArgument

let rec diff (ae, str) =
	match ae with
	| CONST x -> CONST 0
	| VAR x -> 
		if x=str then CONST 1
		else CONST 0
	| POWER (s, i) ->
		if s=str then (
			if i=1 then CONST 1
			else if i=0 then CONST 0
			else TIMES ([CONST i; POWER (s, i-1)]))
		else CONST 0
	| TIMES l -> 
		if l=[] then raise InvalidArgument
		else if (check (l, str))=true then SUM (gettimes (l, str))
		else CONST 0
	| SUM l -> 
		if l=[] then raise InvalidArgument
		else SUM (getsum (l, str))

and check (l, str) = 
	match l with
	| hd::tl -> (
		match hd with
		| VAR x ->
			if x=str then true
			else check (tl, str)
		| POWER (x, i) -> 
			if x=str then true
			else check (tl, str)
		| TIMES x | SUM x -> 
			if check (x, str)=false then check (tl, str)
			else true
		| _ -> check (tl, str))
	| [] -> false

and gettimes (l, str) =
	match l with
	| hd::tl ->
		if tl=[] then [diff (hd, str)]
		else [TIMES ([diff (hd, str)] @ tl)] @ [TIMES ([hd] @ [SUM (gettimes (tl, str))])]
	| [] -> []

and getsum (l, str) = 
	match l with
	| hd::tl -> [diff (hd, str)] @ getsum (tl, str)
	| [] -> []
	
