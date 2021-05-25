type aexp = 
|Const of int
|Var of string
|Power of string * int
|Times of aexp list
|Sum of aexp list

let rec diff : aexp * string -> aexp
= fun(aexp, x) -> match aexp with
Const _ -> Const 0
|Var x -> Const 1
|Power (x, a) -> 
	(match a with
	0 -> Const 0
	|1 -> Const 1
	|2 -> Times [Const 2;Var x]
	|_ -> Times [Const a;Power (x, a-1)])
|Times l ->
	(match l with
	[] -> raise(Failure"NO INPUT")
	|[b] -> diff(b, x)
	|h::t -> Sum [Times (diff(h, x)::t) ;Times (h::[diff(Times t, x)])])
|Sum s ->
	(match s with
	[] -> raise(Failure"NO INPUT")
	|[b] -> diff(b, x)
	|h::t -> Sum [diff(h, x);diff(Sum t, x)])
