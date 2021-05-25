type formula = True
| False
| Not of formula
| AndAlso of formula * formula
| OrElse of formula * formula
| Imply of formula * formula
| Equal of exp * exp
and exp = Num of int
| Plus of exp * exp
| Minus of exp * exp

let rec eval(f)=
	let rec foo(num)=
		match num with
		|Num i -> i
		|Plus(a,b) -> foo(a) + foo(b)
		|Minus(a,b) -> foo(a) - foo(b)
	in
	match f with
	|True -> true
	|False -> false
	|Not g -> not(eval(g))
	|AndAlso(p,q) -> eval(p) && eval(q)
	|OrElse(p,q) -> eval(p) || eval(q)
	|Imply(p,q) -> if(eval(p)) then eval(q) else true
	|Equal(a,b) -> (foo(a) = foo(b))
