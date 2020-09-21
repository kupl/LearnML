type formula = 
  | True
  | False
  | Not of formula
  | AndAlso of formula * formula
  | OrElse of formula * formula
  | Imply of formula * formula
  | Equal of exp * exp

and exp = 
  | Num of int
  | Plus of exp * exp
  | Minus of exp * exp

let imply x = match x with 
         (true,false) -> false
       |   _          -> true;;

let not2 x = match x with
	 true -> false
   | false -> true;;

let and_Also x = match x with
	  (true, true) -> true
	 | _		   -> false;;

let or_Else x = match x with
	  (false, false) -> false
	 | _		   -> true;;

let rec exp : exp -> int
= fun f -> match f with
 | Num n -> n
 | Plus(exp1, exp2) -> exp(exp1) + exp(exp2)
 | Minus(exp1, exp2) -> exp(exp1) - exp(exp2);;

 let equal : int*int -> bool
  = fun (x, y) -> x = y;;

let rec eval : formula -> bool
= fun f -> (*TODO*)
 match f with
 | True -> true
 | False -> false
 | Not(formula) -> not(eval(formula))
 | Imply(formula1, formula2) -> imply(eval(formula1), eval(formula2))
 | AndAlso(formula1, formula2) -> and_Also(eval(formula1), eval(formula2))
 | OrElse(formula1, formula2) -> or_Else(eval(formula1), eval(formula2))
 | Equal(exp1, exp2) -> equal(exp(exp1), exp(exp2));;
  
eval (OrElse(Not(True), Imply(True, Equal(Num 3, Num 8))));;
  