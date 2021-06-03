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

let rec parse e =
match e with
| Num(n) -> n
| Plus(e1,e2) -> parse e1 + parse e2
| Minus(e1,e2) -> parse e1 - parse e2

let rec eval f =
match f with
| True -> true
| False -> false
| Not(f) -> not (eval f)
| AndAlso(a,b) -> (eval a)&&(eval b)
| OrElse(a,b) -> (eval a)||(eval b)
| Imply(a,b) -> if (eval a)=true then eval b else true
| Equal(e1,e2) -> parse e1 = parse e2

