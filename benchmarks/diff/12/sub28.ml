type ae = CONST of int
		| VAR of string
		| POWER of string * int
		| TIMES of ae list
		| SUM of ae list
exception CriticalE
let rec diff (a,str) =
	match a with
	|CONST s -> (CONST 0)
	|VAR s -> if s=str then (CONST 1) else (CONST 0)
	|POWER(s,n) -> if s=str then (TIMES [(CONST n);(POWER(s,(n-1)))])
						  else (CONST 0)
	|TIMES (hd::[]) -> (diff (hd,str))
	|TIMES (hd::tl) -> 
		(let primehd = (diff (hd,str)) in
		 let primetl = (diff ((TIMES tl),str)) in
			if (primehd = (CONST 0)) then 
				if (primetl = (CONST 0)) then (CONST 0)
				else (TIMES [hd;(diff((TIMES tl),str))])
			else if (primetl = (CONST 0)) then (TIMES (primehd::tl))
				else (SUM [(TIMES (primehd::tl));(TIMES [hd;primetl])])
		)
	|TIMES ([])-> (CONST 0)
	|SUM lst -> 
		(let rec summod l =
			match l with
			|[] -> []
			|hd::tl-> (let primehd = (diff (hd,str)) in
									if primehd = (CONST 0) then (summod tl)
									else primehd::(summod tl))
			in
		let result = (summod lst) in
		if (result = []) then (CONST 0) else (SUM result)
		)


