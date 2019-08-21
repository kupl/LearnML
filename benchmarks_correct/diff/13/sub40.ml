type aexp = Const of int
					| Var of string
					| Power of string * int
					| Times of aexp list
					| Sum of aexp list

exception InvalidArgument (* if argument of Times and Sum is an empty list *)

let rec diff = fun (aexp, str) ->
	match aexp with
		Const(i) -> Const 0
		| Var(i) -> if i = str then Const 1 else Const 0
		| Power(i1, i2) -> if i1 = str then (
													if i2 = 1 then Const 1
													else if i2 = 0 then Const 0
													else Times [Const i2; Power (i1, i2 - 1)]
													)
												else Const 0
		| Times(i) -> (match i with
			[] -> raise InvalidArgument
			| h::t -> if t=[] then diff(h, str)
								else Sum [Times ([diff(h, str)]@t); Times [h; diff(Times t, str)]])
		| Sum(i) -> (match i with
			[] -> raise InvalidArgument
			| h::t -> if t=[] then diff(h, str)
								else (Sum [diff(h, str); diff(Sum t, str)]))
