type aexp = Const of int
| Var of string
| Power of string * int
| Times of aexp list
| Sum of aexp list



let rec diff(e,var) = match e with
	Const n -> Const 0
	| Var s -> if (s = var) then Const 1 else Const 0
	| Power(s, n) ->
		if (n == 0) then Const 0
        else if (s = var) then Times([Const n; Power(s, n-1)])
		else Const 0
	(*Times: if there is var x, then only diff that once. and sum them up. else? return Const 0 *)
	| Times l -> 
	let rec mkarr lst var ind cnt = if (cnt < List.length lst) then 
		if (ind == cnt) then [(diff ((List.nth lst cnt), var))] @ (mkarr lst var ind (cnt+1))
		else [(List.nth lst cnt)] @ (mkarr lst var ind (cnt+1))
	else []
	in let fnc ind = (mkarr l var ind 0)
	in let rec implmapi fnc l cnt = if (cnt < List.length l) then [Times(fnc cnt)] @ (implmapi fnc l (cnt+1)) else []
	in Sum (implmapi fnc l 0)
	| Sum l -> let rec fnc = function
			| [] -> []
			| x :: l -> insert (diff(x,var)) (fnc l)
		and insert ele = function
			| [] -> [ele]
			| x :: l -> x :: (l @ [ele])
		in Sum (fnc l)
