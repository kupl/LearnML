exception Exception of string

type exp = X
| INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp


let rec mathemadiga exp = 
let eval exp = mathemadiga exp in

let rec assign (exp,n) = 
let rec sigma a b exp sum = 
 if a>b then sum
 else 
 (sigma (a +. 1.) b exp (sum +. assign(exp,a))) in
 
let rec integral a b exp sum = 
 if (a+.0.1) < b 
 then (integral (a +. 0.1) b exp (sum +. (assign(exp,a) *. 0.1))) 
 else (sum +. (assign(exp,a))*.(b-.a))
 in
 
match exp with
| X -> n
| INT a -> float a
| REAL a -> a
| ADD (e1,e2) -> (assign (e1,n)) +. (assign (e2,n))
| SUB (e1,e2) -> (assign (e1,n)) -. (assign (e2,n))
| MUL (e1,e2) -> (assign (e1,n)) *. (assign (e2,n))
| DIV (e1,e2) -> if (assign (e2,n)) = 0. then raise (Exception " DividedByZero")
else (assign (e1,n)) /. (assign (e2,n))
| SIGMA (e1,e2,e3) -> (sigma (assign (e1,n)) (assign (e2,n)) e3 0.)  
| INTEGRAL (e1,e2,e3) -> if (assign (e1,n)) > (assign (e2,n)) 
then -.(integral (assign (e2,n)) (assign (e1,n)) e3 0.)
else (integral (assign (e1,n)) (assign (e2,n)) e3 0.) in 

let rec sigma a b exp sum = 
 if a>b then sum
 else 
 (sigma (a +. 1.) b exp (sum +. assign(exp,a))) in
 
let rec integral a b exp sum = 
 if (a+.0.1) < b 
 then (integral (a +. 0.1) b exp (sum +. (assign(exp,a) *. 0.1))) 
 else (sum +. (assign(exp,a))*.(b-.a))
 in

match exp with
| INT a -> float a
| REAL a ->  a
| ADD (e1,e2) -> (eval e1) +. (eval e2)
| SUB (e1,e2) -> (eval e1) -. (eval e2)
| MUL (e1,e2) -> (eval e1) *. (eval e2)
| DIV (e1,e2) -> if (eval e2) = 0. then raise (Exception " DividedByZero")
else (eval e1) /. (eval e2)
| SIGMA (e1,e2,e3) -> (sigma (eval e1) (eval e2) e3 0.)  
| INTEGRAL (e1,e2,e3) -> if (eval e1)> (eval e2)
then -.(integral (eval e2) (eval e1) e3 0.)
else (integral (eval e1) (eval e2) e3 0.)
| _ -> raise (Exception "FreevarError")
