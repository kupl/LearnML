type ae = CONST of int
	| VAR of string
	| POWER of string * int
	| TIMES of ae list
	| SUM of ae list

let rec diff : ae * string -> ae = fun (expr,str) ->
	match expr with
	CONST i -> CONST 0
	|VAR s -> if (String.compare str s) != 0 then CONST 0
		  else CONST 1
	|POWER(s,i) ->  if (String.compare str s) != 0 then CONST 0
		 	else TIMES [CONST i; POWER(s,i-1)] 
	|TIMES l -> (match l with
		[] -> CONST 0
		|h::t -> (match t with
			[] -> TIMES [diff(h,str)]
			|_ -> SUM[ TIMES ((diff(h,str))::t); TIMES [h;(diff(TIMES t,str))]]
			)
		)
	|SUM l -> (match l with
		[] -> CONST 0
		|h::t -> (match t with
            [] -> diff(h,str)
            |_ -> SUM [diff(h,str);diff(SUM t,str)]
            )
		)


