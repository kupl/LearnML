
type ae = CONST of int
	| VAR of string
	| POWER of string * int
	| TIMES of ae list
	| SUM of ae list


let rec diff(ae, x) =
	match ae with
	| CONST n -> CONST 0
	| VAR v ->
		if v = x then CONST 1
		else CONST 0
	| POWER(v, n) ->
		if v = x then TIMES [CONST n;POWER(v, n-1)] 
		else CONST 0
(*	| TIMES l -> 
		let tmp1 = 1 in
		let tmp2 = 0 in
		let rec lstRec1 l =
			match l with
			| [] -> CONST 0
			| h::t ->
				match h with
				| CONST n ->
					let tmp1 = tmp1 * n
					lstRec1 t
				| VAR v ->
					if v = x then
						tmp2 = tmp2 + 1
						lstRec1 t
					else lstRec1 t
				| POWER(v, n) ->
					if v = x then 
						tmp2 = tmp2 + n
						lstRec1 t
					else lstRec1 t in
		
		if tmp2 = 0 then CONST 0
		else if tmp2 = 1 then tmp1
		else TIMES [tmp1;POWER(v, tmp2-1);tmp2]
*)
	| SUM l ->
		let rec lstRec2 l =
			match l with
			| [] -> CONST 0
			| h::t -> SUM[diff(h, x);(lstRec2 t)] in
		lstRec2 l
