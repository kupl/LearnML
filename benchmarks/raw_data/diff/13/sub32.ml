type aexp = Const of int
| Var of string
| Power of string * int
| Times of aexp list
| Sum of aexp list

exception InvalidArgument

let rec diff (expr, variable) = 
	match expr with 
		Const(i) -> Const(0)
		| Var(str) -> (
			if (variable = str) then Const(1)
			else Const(0))
		| Power(str, i) ->(
			if (variable = str) then (Times((Const(i))::(Power(str, i-1))::[]))
			else Const(0))
		| Times(l) -> (
			match l with
				[] -> raise InvalidArgument
				| hd::[] -> (diff (hd, variable))
				| hd::tl -> (Sum((Times((diff (hd, variable))::tl))::(Times(hd::(diff ((Times(tl)), variable))::[]))::[]))) 
		| Sum(l) ->(
			match l with
				[] -> raise InvalidArgument
				| hd::[] -> (diff (hd, variable))
				| hd::tl -> (Sum((diff (hd, variable))::(diff ((Sum(tl)), variable))::[])))

