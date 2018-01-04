type formula = TRUE
| FALSE
| NOT of formula
| ANDALSO of formula * formula
| ORELSE of formula * formula
| IMPLY of formula * formula
| LESS of expr * expr
and expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr

let rec eval(f)=
	let rec foo(num)=
		match num with
		|NUM i -> i
		|PLUS(a,b) -> foo(a) + foo(b)
		|MINUS(a,b) -> foo(a) - foo(b)
	in
	match f with
	|TRUE -> true
	|FALSE -> false
	|NOT g -> not(eval(g))
	|ANDALSO(p,q) -> eval(p) && eval(q)
	|ORELSE(p,q) -> eval(p) || eval(q)
	|IMPLY(p,q) -> if(eval(p)) then eval(q) else true
	|LESS(a,b) -> foo(a) < foo(b)
