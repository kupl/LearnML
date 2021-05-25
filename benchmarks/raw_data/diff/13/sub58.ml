type aexp = Const of int
	| Var of string
	| Power of string * int
	| Times of aexp list
	| Sum of aexp list

exception InvalidArgument

let rec diff(aexp,str) =
	match aexp with
	| Const c -> Const (0)
	| Var s -> if(s=str) then Const (1)
		else Const (0)
	| Power (s, c) ->  if(s=str) then Times[Power(s, c-1);Const (c)]
		else Const (0)
	| Times li ->
	(
		match li with
		| [] -> raise InvalidArgument
		| [head] -> diff(head, str)
		| head :: tail -> Sum[Times[diff(head, str);Times(tail)] ; Times[head ; diff(Times(tail), str)]]
	)
	| Sum l ->
	(
		match l with
		| [] -> raise InvalidArgument
		| [head] -> diff(head, str)
		| head :: tail -> Sum[diff(head, str) ; diff(Sum(tail), str)]
	)
