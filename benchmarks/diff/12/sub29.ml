
type ae 
	= CONST of int
	| VAR of string
	| POWER of string * int
	| TIMES of ae list
	| SUM of ae list

exception ERR_IN_TIMES
exception ERR_IN_SSIMP_TIMES 
exception ERR_IN_SSIMP_SUM
let diff (equ, var) =
	
	(*def of simplifying function*)
	let rec simp expr =
		let rec sSimp expr =
			let rec absorbTIMES lst =
			match lst with
				| [] -> []
				| hd::tl -> (let temp = (sSimp hd) in
							(match temp with
							| TIMES l	-> (l @ (absorbTIMES tl))
							
							| _ -> temp::(absorbTIMES tl)
							))
			in 
			let rec absorbSUM lst =
				match lst with
				| []-> []
				| hd::tl -> (let temp = (sSimp hd) in
							(
							match temp with
							| SUM l 	-> (l @ (absorbSUM tl))
							|	_		-> temp::(absorbSUM tl)
							))
			in
			match expr with 
			| CONST i -> (CONST i)
			| VAR str -> (VAR str)
			| POWER (str, pow) 	-> 	(if ((pow > 1) || (pow < 0)) then (POWER (str, pow))
									else if (pow = 1) then (VAR str)
									else (*pow = 0*) (CONST 1)
									)
			| TIMES lst			-> (if ((List.length lst) = 1)then (List.hd lst) 
									else (match lst with
									| [] 	 -> TIMES []
									| hd::tl -> (let mdfLst = (absorbTIMES lst) in 
												if (List.memq (CONST 0) mdfLst) then (CONST 0)
												else (TIMES (List.filter (function elmt -> 
																			(match elmt with
																			| TIMES []	-> false
																			| SUM []	-> false
																			(*| CONST 1	-> false*)
																			| _			-> true)) mdfLst))
												)
									))
			| SUM lst 			-> (if ((List.length lst ) = 1) then (List.hd lst)
									else (match lst with 
									| []	-> SUM []
									| hd::tl -> (let mdfLst = (absorbSUM lst) in
												(SUM (List.filter (function elmt -> (match elmt with
																			| TIMES []	-> false 
																			| SUM []	-> false
																			(*| CONST 0	-> false*)
																			| _			-> true)) mdfLst)))
									)
									)
			in
			let tempRes = (sSimp expr) in
			if (tempRes = expr) then tempRes else (simp tempRes)

		in
									

						
	(*end of simplifying function*)	

let rec sdiff (equ, var) =
	let rec calTimes lst var n =
		let rec popNth lst n lst2 =
			if n = 0 then (lst2, (List.tl lst)) else (popNth (List.tl lst) (n-1) ((List.hd lst)::lst2))
		in
		if (n >= 0) then (let res = sdiff ((List.nth lst n), var) in
						let (bef, aft) = (popNth lst n []) in
						(TIMES (bef @ (res::aft)))::(calTimes lst var (n-1))
						)
		else []
	in
											
	match equ with
	| CONST i	-> (CONST 0)
	| VAR str	-> (if((String.compare str var) = 0) then (CONST 1) else (CONST 0))
	| POWER (str, pow) -> (if(((String.compare str var) = 0) && ((abs pow) >= 1))
							then (TIMES [(CONST pow); POWER (str, (pow-1))])
							else (CONST 0))
	| TIMES lst 	-> (match lst with
						| [] -> raise ERR_IN_TIMES
						| hd::tl -> (SUM (calTimes lst var ((List.length lst)-1)))
							
						)
	| SUM lst		-> (match lst with
						| [] -> (SUM [])
						| hd::tl -> (let res1 = (sdiff (hd, var)) in
									let (SUM res2) = (sdiff ((SUM tl), var)) in
									(SUM (res1::res2))
									)
						)
	in
	(simp (sdiff (equ, var)))
	
