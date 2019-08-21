type aexp = Const of int
| Var of string
| Power of string * int
| Times of aexp list
| Sum of aexp list

exception InvalidArgument

let rec diff (input, x) =
	match input with
	|Const(c) -> Const(0)
	|Var(var) -> if var = x then Const(1) else Const(0) 
	|Power(var,n) -> if n == 0 then diff(Const(1), x) else  if var = x then Times([Const(n); Power(var, n-1)]) else Const(0) 
	|Times(l) ->  let time al = match al with
			| [] -> raise InvalidArgument
               		| hd::[]-> diff(hd,x)
                	| hd::tl->Sum([Times([diff(hd,x)]@tl)]@[Times([hd]@[diff(Times(tl),x)])]) in
			time l 
	|Sum(l) ->    let sum l =  match l with
                        | [] -> raise InvalidArgument
			| h::[]-> diff(h,x)
                        | h::t->Sum([diff(h,x)]@[diff(Sum(t),x)]) in
			sum l

