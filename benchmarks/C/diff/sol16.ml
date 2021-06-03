type aexp = Const of int
		| Var of string
		| Power of string * int
		| Times of aexp list
		| Sum of aexp list


let rec diff(exp, str) = 
	match exp with 
	| Const num -> Const 0
	| Var st -> 
		if st = str then Const 1
		else Const 0
	| Power(st, num) -> 
		if st = str then 
			if num = 1 then Const num
			else Times(Power(st, num - 1)::(Const num)::[])
		else Const 0
	| Sum a -> Sum(diffSum(a, str))
	| Times a -> 
		match a with
		| [] -> Const 0
		| b::[] -> diff(b, str)
		| b::bl -> Sum(
						Times(diff(b, str)::bl)::
Times(b::diff(Times(bl), str)::[])::[])
and diffSum(explist, str) = 
	match explist with
	| [] -> []
	| a::[] -> diff(a, str)::[]
	| a::al -> diff(a, str)::diffSum(al, str)
