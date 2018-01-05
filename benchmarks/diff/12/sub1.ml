exception EMPTYLIST
type ae = CONST of int
	| VAR of string
	| SUM of ae list
	| TIMES of ae list
	| POWER of string * int
 let rec diff (ae, str) =
 
	match ae with
	  
	| CONST a -> CONST 0
	| VAR b -> if str = b then CONST 1
			   else CONST 0
	| POWER (pstr,i) -> if str = pstr then TIMES [CONST i;POWER (pstr, i-1)]
						else CONST 0
	| TIMES alst -> (match alst with
					| [] -> CONST 0
					| (h::[]) -> diff (h,str)
					| (h::t) -> SUM [TIMES (diff (h, str)::t);TIMES (h::[diff(TIMES t,str)])])
	| SUM [] -> CONST 0
	| SUM (h::[]) -> CONST 0
	| SUM (h::t) -> SUM [diff (h, str);diff (SUM t, str)]

	let test= SUM[ TIMES [VAR "a";POWER ("x",2)];TIMES[VAR "b";VAR "x"];VAR "c"]
