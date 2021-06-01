type exp = Num of int | Plus of exp*exp | Minus of exp*exp
type formula = True |  False | Not of formula | AndAlso of formula * formula
                | OrElse of formula * formula | Imply of formula * formula
                | Equal of exp * exp

let rec  calculate : exp -> int = fun exp ->
        match exp with
        | Num i -> i
        | Plus(e1,e2) -> calculate(e1)+calculate(e2)
        | Minus(e1,e2) -> calculate(e1)-calculate(e2)
let rec eval : formula -> bool = fun formula ->
        match formula with
        | True -> true
        | False -> false
        | Not f -> not(eval(f))
        | AndAlso (f1,f2) -> eval(f1)&&eval(f2)
        | OrElse (f1,f2) -> eval(f1)||eval(f2)
        | Imply (f1,f2) -> (not(eval(f1)))||eval(f2)
        | Equal (e1,e2) -> calculate(e1)=calculate(e2)
(*
let f1 : formula = AndAlso(Equal(Num 3,Num 4),Equal(Plus(Num 1,Num 2),Num 1))
let f2 : formula = OrElse(False,Not(Equal(Num 10,Num 9)))
let f3 : formula = Imply(f1,f2)
let b1 : bool = eval(f1)
let b2 : bool = eval(f2)
let b3 : bool = eval(f3)
let _ = print_endline(string_of_bool b1)
let _ = print_endline(string_of_bool b2)
let _ = print_endline(string_of_bool b3)

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


