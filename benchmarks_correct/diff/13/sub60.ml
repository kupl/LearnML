type ae = CONST of int
| VAR of string
| POWER of string * int
| TIMES of ae list
| SUM of ae list

exception InvalidArgument

let rec diff (input, x) =
	match input with
	|CONST(c) -> CONST(0)
	|VAR(var) -> if var = x then CONST(1) else CONST(0) 
	|POWER(var,n) -> if n == 0 then diff(CONST(1), x) else  if var = x then TIMES([CONST(n); POWER(var, n-1)]) else CONST(0) 
	|TIMES(l) ->  let time al = match al with
			| [] -> raise InvalidArgument
               		| hd::[]-> diff(hd,x)
                	| hd::tl->SUM([TIMES([diff(hd,x)]@tl)]@[TIMES([hd]@[diff(TIMES(tl),x)])]) in
			time l 
	|SUM(l) ->    let sum l =  match l with
                        | [] -> raise InvalidArgument
			| h::[]-> diff(h,x)
                        | h::t->SUM([diff(h,x)]@[diff(SUM(t),x)]) in
			sum l

