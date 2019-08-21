(*2009-11718 2-2*)

exception InvalidArgument

type aexp = Const of int
		| Var of string
		| Power of string * int
		| Times of aexp list
		| Sum of aexp list

let rec diff (aexp, str) =
	let rec differ (aexp, str) =
		match aexp with
		| Const a -> Const 0
		| Var str1 -> if str=str1 then
					Const 1
					else Const 0
		| Power (str1, a) -> if str=str1 then
						Times ([Const a]@[Power (str1, a-1)])
						else Const 0
		| Times l -> if l=[] then raise InvalidArgument
					else Sum (times (l, str, []))
		| Sum l -> if l=[] then raise InvalidArgument
					else Sum (sum (l, str))

	and times (aexp, str, result) =
		match aexp with
		| hd::tl -> if List.length aexp = List.length result then
					result
					else (times ((tl@[hd]), str, result@[Times ([diff (hd, str)]@tl)]))
		| [] -> []

	and sum (aexp, str) =
		match aexp with
		| hd::tl -> ([(diff (hd, str))]@(sum (tl, str)))
		| [] -> [] in

	(differ (aexp, str))
