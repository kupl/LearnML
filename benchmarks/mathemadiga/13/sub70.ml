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

let galculator arg = 
let rec auxeval (arg,env) = match (arg,env) with
| (X, []) -> raise FreeVariable
| (X, hd::tl) -> hd
| (INT v, env) -> float_of_int(v)
| (REAL v,env) -> v
| (ADD(u, v),env) -> auxeval(u, env) +. auxeval(v, env)
| (SUB(u, v),env) -> auxeval(u, env) -. auxeval(v, env)
| (MUL(u, v),env) -> auxeval(u, env) *. auxeval(v, env)
| (DIV(u, v),env) -> auxeval(u, env) /. auxeval(v, env)
| (SIGMA(a, b, f),env) -> 
if auxeval(a, env) < auxeval(b, env) then 
auxeval(f, [auxeval(a, env)]) +. auxeval(SIGMA(ADD(INT 1, a), b, f), env)
else if auxeval(a, env) = auxeval(b, env) then auxeval(f, [auxeval(a, env)])
else 0.
| (INTEGRAL(a, b, f),env) -> 

let rec integral_aux (l, u, e) = 
if (u -. l) <= 0.0 then 0.0
else if (u -. l) <= 0.1 then 0.0
else ((auxeval (e, (l::env))) *. 0.1) +. (integral_aux ((l +. 0.1), u, e))
in if auxeval(a, env) > auxeval(b, env) then 
-. integral_aux(auxeval(b,env), auxeval(a,env), f)
else
integral_aux (auxeval(a,env), auxeval(b,env), f)

in auxeval(arg, [])