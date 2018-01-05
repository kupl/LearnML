type exp = X
|INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp

exception FreeVariable

let rec formulator expr = match expr with
X -> (fun x -> x)
| INT(n) -> (fun x -> float_of_int n)
| REAL(f) -> (fun x -> f)
| ADD(a,b) -> 
(fun x -> ((formulator a) x) +. ((formulator b) x))
| SUB(a,b) -> 
(fun x -> ((formulator a) x) -. ((formulator b) x))
| MUL(a,b) ->
(fun x -> ((formulator a) x) *. ((formulator b) x))
| DIV(a,b) -> 
(fun x -> ((formulator a) x) /. ((formulator b) x))


let rec sum a b expr acm = 
if(a<=b) 
then (sum a (b-1) expr 
(acm+.((formulator expr) (float_of_int b))))
else acm

let rec itg a b expr acm =
if(a+.0.1<=b) 
then (itg (a+.0.1) b expr 
(acm +. (((formulator expr) a)/.10.0)  ))
else if(a -. 0.1 >= b) then -.(itg b a expr acm)
else acm
				
let rec galculator expr = match expr with
INT(n) -> float_of_int n
| X -> raise (FreeVariable)
| REAL(f) -> f
| ADD(a,b) -> (galculator a)+.(galculator b)
| SUB(a,b) -> (galculator a)-.(galculator b)
| MUL(a,b) -> (galculator a)*.(galculator b)
| DIV(a,b) -> (galculator a)/.(galculator b) 
| SIGMA(a,b,c) -> 
let aprime = int_of_float (galculator a) in 
let bprime = int_of_float(galculator b) in 
(sum aprime bprime c 0.0)
| INTEGRAL(a,b,c) -> 
itg (galculator a) (galculator b) c 0.0

