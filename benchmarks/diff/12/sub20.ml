(* HW2 exercise2 2009-11697 Kim HyunJoon *)
(* Mathemadiga *)

type ae = CONST of int
	| VAR of string
	| POWER of string * int
	| TIMES of ae list
	| SUM of ae list

let rec diff : ae * string -> ae =
	fun (exp, str) ->
	let rec partialDiff lst n =
		match lst with
		| [] -> []
		| hd::tl -> if n = 0 then (myDiff hd)::tl else hd::(partialDiff tl (n-1))
	and myTimes lst nth =
		match lst with
		| [] -> []
		| _ -> if nth < (List.length lst) then (TIMES (partialDiff lst nth))::(myTimes lst (nth+1)) else []
	and myDiff exp = 
		match exp with
		| CONST n -> (CONST 0)
		| VAR var ->
			if var = str then (CONST 1)
			else (CONST 0)	
		| POWER (var, n) -> 
			if var = str then if n = 0 then (CONST 0) else (TIMES [(CONST n); (POWER (var, (n-1)))])
			else (CONST 0)
		| TIMES lst -> (SUM (myTimes lst 0))
		| SUM lst -> (SUM (List.map myDiff lst))
	in
	let rec removeZero exp =
		match exp with
		| TIMES lst -> if List.mem (CONST 0) lst then (CONST 0) else (TIMES (List.map removeZero lst))
		| SUM lst -> SUM (List.map removeZero (List.filter (fun exp -> if exp != (CONST 0) then true else false) lst))
		| _ -> exp 
	in
	(removeZero (myDiff exp))


(*
let one = CONST 1
let x = VAR "x"
let x2 = POWER ("x", 2)
let a = VAR "a"
let b = VAR "b"
let c = VAR "c"
let eq = SUM [(TIMES [a;x2]);(TIMES [b;x]);c]
*)

