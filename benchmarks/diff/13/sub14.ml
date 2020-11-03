type aexp = Const of int
	| Var of string
	| Power of string * int
	| Times of aexp list
	| Sum of aexp list

exception InvalidArgument

let diff (a, b) = 
	let rec realdiff (a, b) = 
		match a with
		| Const x -> Const 0
		| Var x -> if x=b then Const 1
			   else Const 0
		| Power (x, y) -> if x=b then Times [Const y;Power (x, y-1)]
				  else Const 0
		| Times lst -> (match lst with
				| hd::[] -> realdiff (hd, b)
				| hd::tl -> Sum [Times [hd;realdiff(Times tl, b)] ; Times [realdiff(hd, b);Times tl]]
				| [] -> raise InvalidArgument
			       )
		| Sum lst -> (match lst with
				| hd::[] -> realdiff (hd, b)
				| hd::tl -> Sum[realdiff(hd, b);realdiff(Sum tl, b)]
				| [] -> raise InvalidArgument
			     )
	in
	let maketime (a, b) = 
		match b with
		| Times lst -> Times (a::lst)
		| _ -> Times (a::b::[])
	in
	let makesum (a, b) = 
		match b with
		| Sum lst -> Sum (a::lst)
		| _ -> Sum (a::b::[])
	in
	let rec simplifier a = 
		match a with
		| Const x -> Const x
		| Var x -> Var x
		| Power (x, y) -> Power (x, y)
		| Times lst -> (match lst with
				| hd::[] -> simplifier hd
				| hd::tl -> if (simplifier (Times tl))=(Const 1) then simplifier hd
					    else if (simplifier (Times tl))=(Const 0) then Const 0
					    else if (simplifier hd)=(Const 0) then Const 0
					    else if (simplifier hd)=(Const 1) then simplifier (Times tl)
					    else maketime (simplifier hd, simplifier (Times tl))
				| [] -> raise InvalidArgument
				)
		| Sum lst -> (match lst with
				| hd::[] -> simplifier hd
				| hd::tl -> if (simplifier (Sum tl))=(Const 0) then simplifier hd
					    else if (simplifier hd)=(Const 0) then simplifier (Sum tl)
					    else makesum (simplifier hd, simplifier (Sum tl))
				| [] -> raise InvalidArgument
			     )
	in
	simplifier (realdiff (a, b))
		   
