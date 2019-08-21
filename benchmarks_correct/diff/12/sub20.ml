(* HW2 exercise2 2009-11697 Kim HyunJoon *)
(* Mathemadiga *)

type aexp = Const of int
	| Var of string
	| Power of string * int
	| Times of aexp list
	| Sum of aexp list

let rec diff : aexp * string -> aexp =
	fun (exp, str) ->
	let rec partialDiff lst n =
		match lst with
		| [] -> []
		| hd::tl -> if n = 0 then (myDiff hd)::tl else hd::(partialDiff tl (n-1))
	and myTimes lst nth =
		match lst with
		| [] -> []
		| _ -> if nth < (List.length lst) then (Times (partialDiff lst nth))::(myTimes lst (nth+1)) else []
	and myDiff exp = 
		match exp with
		| Const n -> (Const 0)
		| Var var ->
			if var = str then (Const 1)
			else (Const 0)	
		| Power (var, n) -> 
			if var = str then if n = 0 then (Const 0) else (Times [(Const n); (Power (var, (n-1)))])
			else (Const 0)
		| Times lst -> (Sum (myTimes lst 0))
		| Sum lst -> (Sum (List.map myDiff lst))
	in
	let rec removeZero exp =
		match exp with
		| Times lst -> if List.mem (Const 0) lst then (Const 0) else (Times (List.map removeZero lst))
		| Sum lst -> Sum (List.map removeZero (List.filter (fun exp -> if exp != (Const 0) then true else false) lst))
		| _ -> exp 
	in
	(removeZero (myDiff exp))


(*
let one = Const 1
let x = Var "x"
let x2 = Power ("x", 2)
let a = Var "a"
let b = Var "b"
let c = Var "c"
let eq = Sum [(Times [a;x2]);(Times [b;x]);c]
*)

