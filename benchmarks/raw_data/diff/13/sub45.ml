type aexp = Const of int
	| Var of string
	| Power of string * int
	| Times of aexp list
	| Sum of aexp list

exception InvalidArgument

let rec diff (eq, var) =
	match eq with
		| Const n 	-> Const 0
		| Var x		-> if x = var then Const 1
				   else Const 0
		| Power (x, n)	-> if x = var then Times [Const n; Power (x, n-1)]
				   else Const 0
		| Times lst 	->
			(match lst with
				| [] 		-> raise InvalidArgument
				| head :: []	-> diff (head, var)
				| head :: tail 	-> Sum [(Times ((diff(head, var)) :: tail)); (Times [head; (diff (Times tail, var))])])
		| Sum lst	->
			(match lst with
				| []		-> raise InvalidArgument
				| head :: []	-> diff (head, var)
				| head :: tail  -> Sum [(diff(head, var)); (diff(Sum tail, var))])
