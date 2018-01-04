type ae = CONST of int
| VAR of string
| POWER of string * int
| TIMES of ae list
| SUM of ae list



let rec diff(e,var) = match e with
	CONST n -> CONST 0
	| VAR s -> if (s = var) then CONST 1 else CONST 0
	| POWER(s, n) ->
		if (n == 0) then CONST 0
        else if (s = var) then TIMES([CONST n; POWER(s, n-1)])
		else CONST 0
	(*TIMES: if there is var x, then only diff that once. and sum them up. else? return CONST 0 *)
	| TIMES l -> 
	let rec mkarr lst var ind cnt = if (cnt < List.length lst) then 
		if (ind == cnt) then [(diff ((List.nth lst cnt), var))] @ (mkarr lst var ind (cnt+1))
		else [(List.nth lst cnt)] @ (mkarr lst var ind (cnt+1))
	else []
	in let fnc ind = (mkarr l var ind 0)
	in let rec implmapi fnc l cnt = if (cnt < List.length l) then [TIMES(fnc cnt)] @ (implmapi fnc l (cnt+1)) else []
	in SUM (implmapi fnc l 0)
	| SUM l -> let rec fnc = function
			| [] -> []
			| x :: l -> insert (diff(x,var)) (fnc l)
		and insert ele = function
			| [] -> [ele]
			| x :: l -> x :: (l @ [ele])
		in SUM (fnc l)
