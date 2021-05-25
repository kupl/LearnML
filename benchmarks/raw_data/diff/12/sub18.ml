(* ex2 *)
type aexp = Const of int
		| Var of string
		| Power of string * int
		| Times of aexp list
		| Sum of aexp list

let rec diff (ex, var) = 
	let diffSum e = diff (e, var) in
	
	let replace lst n elem = 
		let rec get_result res prev = 
			match prev with
			  [] -> res
			| hd::tl -> if (List.length res) = n then res@[elem]@tl
						else get_result (res@[hd]) tl in
		get_result [] lst in
		
	let diffTimes lst = 
		let rec looper i n = 
			if i = n then []
			else (Times (replace lst i (diff (List.nth lst i, var))))::(looper (i + 1) n) in
		Sum (looper 0 (List.length lst)) in
		
	
	match ex with
	  Const i -> Const 0
	| Var v -> if v = var then Const 1 else Const 0
	| Power (v, i) -> if v = var then
					      if i = 1 then Const i
						  else Times [Const i; Power (v, i - 1)]
					  else Const 0
	| Times lst -> diffTimes lst
	| Sum lst -> Sum (List.map diffSum lst)