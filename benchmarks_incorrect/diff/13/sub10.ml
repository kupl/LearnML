exception InvalidArgument

type aexp = Const of int
	| Var of string
	| Power of string * int
	| Times of aexp list
	| Sum of aexp list

let diff : aexp * string -> aexp = fun (e, str) ->
	let rec diff_sub (e, str) =
		match e with
		| Const n -> Const 0
		| Var s -> 
		    if s = str then Const 1
		    else Const 0
		| Power (str, n) -> Times [Const n; Power (str, (n-1))]
		| Times lst -> ( 
			match lst with
			| [] -> raise InvalidArgument
			| a::[] -> diff_sub (a, str)
			| a::t -> Sum [Times [diff_sub (a, str); Times t]; Times[a; diff_sub (Times t, str)]]
		)
		| Sum lst -> 
			match lst with
			| [] -> raise InvalidArgument
			| a::[] -> diff_sub (a, str)
			| a::t -> Sum [diff_sub (a, str); diff_sub ((Sum t), str)]
	in
	diff_sub (e, str)
