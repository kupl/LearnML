type aexp = Const of int
		| Var of string
		| Power of string * int
		| Times of aexp list
		| Sum of aexp list
exception CriticalE
let rec diff (a,str) =
	match a with
	|Const s -> (Const 0)
	|Var s -> if s=str then (Const 1) else (Const 0)
	|Power(s,n) -> if s=str then (Times [(Const n);(Power(s,(n-1)))])
						  else (Const 0)
	|Times (hd::[]) -> (diff (hd,str))
	|Times (hd::tl) -> 
		(let primehd = (diff (hd,str)) in
		 let primetl = (diff ((Times tl),str)) in
			if (primehd = (Const 0)) then 
				if (primetl = (Const 0)) then (Const 0)
				else (Times [hd;(diff((Times tl),str))])
			else if (primetl = (Const 0)) then (Times (primehd::tl))
				else (Sum [(Times (primehd::tl));(Times [hd;primetl])])
		)
	|Times ([])-> (Const 0)
	|Sum lst -> 
		(let rec summod l =
			match l with
			|[] -> []
			|hd::tl-> (let primehd = (diff (hd,str)) in
									if primehd = (Const 0) then (summod tl)
									else primehd::(summod tl))
			in
		let result = (summod lst) in
		if (result = []) then (Const 0) else (Sum result)
		)


