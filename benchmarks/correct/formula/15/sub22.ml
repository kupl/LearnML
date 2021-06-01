type formula = True
        |False
        |Not of formula
        |AndAlso of formula * formula
        |OrElse of formula * formula
        |Imply of formula * formula
        |Equal of exp * exp
and exp = Num of int
        |Plus of exp * exp
        |Minus of exp * exp

let rec eval f : bool =
	match f with
	True	-> true
	|False	-> false
	|Not(form)		-> not (eval form)
	|AndAlso(form, latt)	-> (eval form) && (eval latt)
	|OrElse(form, latt)	-> (eval form) || (eval latt)
	|Imply(form, latt)	-> not (eval form) || (eval latt)
	|Equal(form, latt)	-> if getval form = getval latt then true else false
and getval s = 
	match s with
	Num(n)	-> n
	|Plus(n, m)	-> (getval n) + (getval m)
	|Minus(n, m)	-> (getval n) - (getval m)
