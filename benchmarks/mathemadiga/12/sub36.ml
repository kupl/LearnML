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

let mathemadiga expr =
let rec real_math expr flag num =
let rec sigma a b exp =
if (b -. a) < 0.0 then 0.0
	else (real_math exp 1 a) +. (sigma (a+.1.0) b exp)
in
let sigma_test a b exp =
if (a > b) then 0.0
	else (sigma a b exp)
in
let rec inte down up exp =
if(up -. down) <= 0.1 then (up -. down) *. (real_math exp 1 down)
	else ((real_math exp 1 down) *. 0.1) +. (inte (down +. 0.1) up exp)
in
let inte_test down up exp =
if(down > up ) then 0.0 -. (inte up down exp)
	else (inte down up exp)
in
match expr, flag, num with
|X, 1, n -> n
|X, _, _ -> raise FreeVariable
|INT i, _, _ -> float_of_int i
|REAL f, _, _ -> f
|ADD (exp1, exp2), fl, n -> (real_math exp1 fl n) +. (real_math exp2 fl n)
|SUB (exp1, exp2), fl, n -> (real_math exp1 fl n) -. (real_math exp2 fl n)
|MUL (exp1, exp2), fl, n -> (real_math exp1 fl n) *. (real_math exp2 fl n)
|DIV (exp1, exp2), fl, n -> (real_math exp1 fl n) /. (real_math exp2 fl n)
|SIGMA (exp1, exp2, exp3), fl, n -> (sigma_test (float_of_int(int_of_float (real_math exp1 fl n))) (float_of_int(int_of_float (real_math exp2 fl n))) exp3)
|INTEGRAL (exp1, exp2, exp3), fl, n -> (inte_test (real_math exp1 fl n) (real_math exp2 fl n) exp3)
in
(real_math expr 0 0.0)
