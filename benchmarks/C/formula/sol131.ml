
type formula = True
			|	False
			|	Not of formula
			|	AndAlso of formula * formula
			|	OrElse of formula * formula
			|	Imply of formula * formula
			|	Equal of exp * exp
and exp = Num of int
		|	Plus of exp * exp
		|	Minus of exp * exp
;;

let rec eval form =
	let rec int_of_exp exp_in =
		match exp_in with
	|	Num x -> x
	|	Plus (exp1, exp2) -> int_of_exp(exp1) + int_of_exp(exp2)
	|	Minus (exp1, exp2) -> int_of_exp(exp1) - int_of_exp(exp2)
	in
	match form with
	|	True -> true
	|	False -> false
	|	Not form1 -> not (eval form1)
	|	AndAlso (form1, form2) -> (eval form1) && (eval form2)
	|	OrElse (form1, form2) -> (eval form1) || (eval form2)
	|	Imply (form1, form2) -> not (eval form1) || (eval form2)
	|	Equal (exp1, exp2) -> int_of_exp(exp1) = int_of_exp(exp2)
	;;

(* exercise test
Printf.printf "eval : %b\n" (eval (Not(Equal(Num 10, Num 2))));;
exercise *)
