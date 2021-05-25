type formula=
    True
	|False
	|Not of formula
	|AndAlso of formula*formula
	|OrElse of formula*formula
	|Imply of formula*formula
	|Equal of exp*exp
and exp=
    |Num of int
	|Plus of exp*exp
	|Minus of exp*exp

let rec exp_eval:exp->int=
    fun ex->
	    match ex with
		|Num n -> n
		|Plus (ln,rn) -> (exp_eval ln)+(exp_eval rn)
	    |Minus (ln,rn)-> (exp_eval ln)-(exp_eval rn)

let rec eval:formula->bool=
    fun fo->
	    match fo with
		| True -> true
		| False -> false
		| Not f -> not((eval f))
	    | AndAlso (lf,rf) -> (eval lf)&&(eval rf)
		| OrElse (lf,rf) -> (eval lf)||(eval rf)
		| Imply (lf,rf) -> not((eval lf))||(eval rf)
		| Equal (ln,rn) -> (exp_eval ln)=(exp_eval rn)
