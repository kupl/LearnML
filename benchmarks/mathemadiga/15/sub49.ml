type exp = X
 | INT of int
 | REAL of float
 | ADD of exp * exp
 | SUB of exp * exp
 | MUL of exp * exp
 | DIV of exp * exp
 | SIGMA of exp * exp * exp
 | INTEGRAL of exp * exp * exp
exception FreeVariable

let abs : float -> float = fun a -> 
 if a>0.0 then a 
 else a*.(-1.0)

let rec ssigma : int*int*(float->float)->float = fun (a,b,f) ->
 if a>b then 0.0
 else f(float_of_int a)+.ssigma(a+1,b,f)

let rec iintegral : float*float*(float->float)->float = fun (a,b,f) ->
 if abs(a-.b)<0.1 then 0.0
 else if a>b then iintegral(b,a,f)
 else 0.1*.f(a)+.iintegral(a+.0.1,b,f)

let rec functionize : exp -> (float->float) = fun e ->
 match e with
 | X -> (fun x -> x)
 | INT(i) -> (fun x -> (float_of_int i))
 | REAL(r) -> (fun x -> r)
 | ADD(f1,f2) -> (fun x -> (functionize f1 x)+.(functionize f2 x))
 | SUB(f1,f2) -> (fun x -> (functionize f1 x)-.(functionize f2 x))
 | MUL(f1,f2) -> (fun x -> (functionize f1 x)*.(functionize f2 x))
 | DIV(f1,f2) -> (fun x -> (functionize f1 x)/.(functionize f2 x))
 | SIGMA(f1,f2,f3) -> (fun x -> ssigma(int_of_float (functionize f1 x),int_of_float (functionize f2 x),functionize f3))
 | INTEGRAL(f1,f2,f3) -> (fun x -> iintegral((functionize f1 x),(functionize f2 x), functionize f3))

let rec galculator : exp -> float = fun e ->
 match e with
 | X -> raise FreeVariable
 | INT(i) -> float_of_int i
 | REAL(r) -> r
 | ADD(e1,e2) -> (galculator e1)+.(galculator e2)
 | SUB(e1,e2) -> (galculator e1)-.(galculator e2)
 | MUL(e1,e2) -> (galculator e1)*.(galculator e2)
 | DIV(e1,e2) -> if (galculator e2)=0.0 then raise Division_by_zero
  else (galculator e1)/.(galculator e2)
 | SIGMA(e1,e2,f) -> ssigma(int_of_float (galculator e1),int_of_float (galculator e2),functionize f)
 | INTEGRAL(e1,e2,f) -> iintegral(galculator e1,galculator e2,functionize f)