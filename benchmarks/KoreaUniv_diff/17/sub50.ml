(*problem 4*)
type aexp =
	|Const of int
	|Var of string
	|Power of string * int
	|Times of aexp list
	|Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (e,x) -> match e with
				|Const c -> Const 0
				|Var v-> if v = x then Const 1 else Const 0
				|Power (v,c) -> if v = x then Times [Const c; Power (v, (c-1))] else Const 0
				|Times [] -> Const 0
				|Times (hd::tl) -> if tl = [] then diff(hd,x)
								else Sum [Times (diff (hd, x):: tl); Times [hd; diff (Times tl, x)]]
				|Sum [] -> Const 0
				|Sum (hd::tl) -> if tl = [] then diff(hd,x)
								else Sum [diff (hd, x); diff(Sum tl, x)]
