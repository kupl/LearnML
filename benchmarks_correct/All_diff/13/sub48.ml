type aexp = Const of int
	| Var of string
	| Power of string * int
	| Times of aexp list
	| Sum of aexp list

exception InvalidArgument

let rec diff(expr, var) = 
	match expr with
	| Const(x) -> Const(0)
	| Var(x) ->(
		if(x = var) then Const(1)
		else Const(0)
	)
	| Power(x, pow) ->(
		if(x = var) then 
			if(pow != 0) then Times([Const(pow); Power(x, pow - 1)])
			else Const(0)
		else Const(0)
	)
	| Times(xlist) -> (
		if(xlist = []) then raise InvalidArgument
		else if(List.length xlist = 1) then diff(List.hd xlist, var)
		else Sum([Times([diff(List.hd xlist, var); Times(List.tl xlist)]); Times([List.hd xlist; diff(Times(List.tl xlist), var)])])
	)
	| Sum(xlist) -> (
		if(xlist = []) then raise InvalidArgument
		else if(List.length xlist = 1) then diff(List.hd xlist, var)
		else Sum([diff(List.hd xlist, var); diff(Sum(List.tl xlist), var)])
	)

