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

let rec g (e, t, v) = match (e, t, v) with
| (X, tf, va) -> (if tf then va else (raise FreeVariable))
| (INT a,_,_) -> float_of_int a
| (REAL b,_,_) -> b
| (ADD (e1, e2), tf, va) -> (g (e1, tf, va)) +. (g (e2, tf, va))
| (SUB (e1, e2), tf, va) -> (g (e1, tf, va)) -. (g (e2, tf, va))
| (MUL (e1, e2), tf, va) -> (g (e1, tf, va)) *. (g (e2, tf, va))
| (DIV (e1, e2), tf, va) -> (g (e1, tf, va)) /. (g (e2, tf, va))
| (SIGMA (a, b, ex), tf, va) -> 
	gsigma ((int_of_float (g (a, tf, va))), (int_of_float (g (b, tf, va))), ex)
| (INTEGRAL (a, b, ex), tf, va) -> 
	ginte ((g (a, tf, va)), (g (b, tf, va)), ex)

and gsigma (a, b, ex) = (* a and b are int *) 
	(if (a > b) then 0. else ( (g (ex, true, (float_of_int a))) +. (gsigma ((a+1), b, ex))) )

and ginte (c, d, ex) = (if (c > d) then (0. -. (ginte (d, c, ex))) else
	(if (c +. 0.1 > d) then 0. else ( ((g (ex, true, c)) *. 0.1) +. (ginte ((c +. 0.1), d, ex))) ))

let galculator e = g (e, false, 0.)