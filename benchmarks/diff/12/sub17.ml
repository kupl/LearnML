(* 20008-11874 SUJEE LEE execise 2 *)

type ae = CONST of int 
| VAR of string
| POWER of string * int 
| TIMES of ae list
| SUM of ae list


let rec diff (ae,str) =
	match ae with
		| CONST c -> CONST 0
		| VAR v -> if str = v then CONST 1 else CONST 0
		| POWER(v,p) ->
			if v = str
			then TIMES [CONST p; POWER(v,p-1)] else CONST 0
		| TIMES ael -> (* [var X; var X] check *)
			(match ael with
				| [] -> TIMES []
				| hd::[] -> diff (hd,str)
				| hd::tl -> SUM [ TIMES ((diff (hd,str))::tl) ; TIMES [hd; diff (TIMES tl,str)] ]
				)
		| SUM ael ->
			let diffwithstr e = diff (e,str) in
			SUM (List.map diffwithstr ael)
