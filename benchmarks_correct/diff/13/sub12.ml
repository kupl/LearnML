exception InvalidArgument

type aexp = Const of int
	| Var of string
	| Power of string * int
	| Times of aexp list
	| Sum of aexp list

let rec diff (aexp,x) =
	match aexp with Const _ -> Const 0
	| Var v -> if (v=x) then Const 1 else Const 0
	| Power (v,p) ->  if (v=x) then Times [Const p; Power(v,p-1)] else Const 0
	| Times lst ->
		(
		match lst with [] -> raise InvalidArgument
		| head::[] -> diff (head, x)
		| head::tail -> Sum [ Times ((diff (head, x))::tail); Times (head::[diff(Times tail,x)]) ]
		)
	| Sum lst ->
		(
		match lst with [] -> raise InvalidArgument
		| head::[] -> diff (head, x)
		| head::tail -> Sum ((diff (head, x))::[diff(Sum tail,x)])
		)
