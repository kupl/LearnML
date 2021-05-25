(* 2008-11874 Lee, Sujee *)
(* EXERCISE 5 *)
	
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


let rec eval formula = (* eval : formula -> bool = =fun> *)
	let rec calc exp =
		match exp with
			| Num i -> i
			| Plus(e1,e2) -> (calc e1) + (calc e2)
			| Minus(e1,e2) -> (calc e1) - (calc e2)
		in
	match formula with
		| True -> true
		| False -> false
		| Not f -> not (eval f)
		| AndAlso(f1,f2) -> (eval f1) && (eval f2)
		| OrElse(f1,f2) -> (eval f1) || (eval f2)
		| Imply(f1,f2) -> not (((eval f1)=true) && ((eval f2)=false)) 
		(* false only when f1 is true but f2 is false.*)
		| Equal(e1,e2) -> (calc e1) = (calc e2)

(*	
let result5 = eval(Equal(Plus(Num 5, Num 5), Minus(Num 20, Num 7)))
let _ =
	print_string "EXERCISE 5 : ";
	print_string (string_of_bool result5);
	print_newline()
	*)