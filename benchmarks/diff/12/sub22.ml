(* 2009-11718 2-2*)

type ae = CONST of int
		| VAR of string
		| POWER of string * int
		| TIMES of ae list
		| SUM of ae list

let rec diff (ae, str) =
	let rec differ (ae, str) =
		match ae with
		| CONST a -> CONST 0
		| VAR str1 -> if str=str1 then
					CONST 1
					else CONST 0
		| POWER (str1, a) -> if str=str1 then
					TIMES ([CONST a]@[POWER (str1, a-1)])
					else CONST 0
		| TIMES l -> SUM (times (l, str, []))
		| SUM l -> SUM (sum (l, str))


	and times (ae, str, result) =
		match ae with
		|hd::tl -> if List.length ae = List.length result then
				result
				else (times ((tl@[hd]), str, result@[TIMES ([diff (hd, str)]@tl)]))
		|[] -> []

	and sum (ae, str) =
		match ae with
		|hd::tl -> ([(diff (hd, str))] @ (sum (tl, str)))
		|[] -> [] in

	(differ (ae, str))

