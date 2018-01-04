(* ex2 *)
type ae = CONST of int
		| VAR of string
		| POWER of string * int
		| TIMES of ae list
		| SUM of ae list

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
			else (TIMES (replace lst i (diff (List.nth lst i, var))))::(looper (i + 1) n) in
		SUM (looper 0 (List.length lst)) in
		
	
	match ex with
	  CONST i -> CONST 0
	| VAR v -> if v = var then CONST 1 else CONST 0
	| POWER (v, i) -> if v = var then
					      if i = 1 then CONST i
						  else TIMES [CONST i; POWER (v, i - 1)]
					  else CONST 0
	| TIMES lst -> diffTimes lst
	| SUM lst -> SUM (List.map diffSum lst)