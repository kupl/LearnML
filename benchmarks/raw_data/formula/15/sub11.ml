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

let rec _eval : exp -> int = fun e ->
	match e with
	| Num i -> i
	| Plus (e1,e2) -> _eval(e1) + _eval(e2)
	| Minus (e1,e2) -> _eval(e1) - _eval(e2)

let rec eval : formula -> bool = fun f ->
	match f with
	| True -> true
	| False -> false
	| Not f_ -> not(eval(f_))
	| AndAlso (f_, f__) ->  eval(f_) && eval(f__)
	| OrElse (f_, f__) -> eval(f_) || eval(f__)
	| Imply (f_, f__) -> not(eval(f_)) || (eval(f_) && eval(f__))
	| Equal (e_, e__) -> _eval(e_) = _eval(e__)

(*
let _ = 
let print_bool x = 
print_endline (string_of_bool x) in 
print_bool (true = eval True); 
print_bool (false = eval False); 
print_bool (false = eval (Not True)); 
print_bool (true = eval (Not False)); 
print_bool (true = eval (AndAlso (True, True))); 
print_bool (false = eval (AndAlso (True, False))); 
print_bool (false = eval (AndAlso (False, True))); 
print_bool (false = eval (AndAlso (False, False))); 
print_bool (true = eval (OrElse (True, True))); 
print_bool (true = eval (OrElse (True, False))); 
print_bool (true = eval (OrElse (False, True))); 
print_bool (false = eval (OrElse (False, False))); 
print_bool (false = eval (Imply (True, False))); 
print_bool (true = eval (Imply (True, True))); 
print_bool (true = eval (Imply (False, True))); 
print_bool (true = eval (Imply (False, False))); 
print_bool (true = eval (Equal (Num 3, Num 5))); 
print_bool (false = eval (Equal (Num 3, Num 3))); 
print_bool (false = eval (Equal (Num 3, Num 1))); 
print_bool (false = eval 
(Equal (Plus (Num 3, Num 4), Minus (Num 5, Num 1)))); 
print_bool (true = eval 
(Equal (Plus (Num 10, Num 12), Minus (Num 10, Num (-13))))); 

*)
