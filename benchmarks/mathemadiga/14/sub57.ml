type exp =
  X
| INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp

exception FreeVariable

let galculator =
function e ->
let rec galculator_var e table =
match e with
  X ->
  	(match table with
	  [] -> raise FreeVariable
	| x::t -> x)
| INT i -> Pervasives.float_of_int i
| REAL f -> f
| ADD (e1, e2) -> (galculator_var e1 table) +. (galculator_var e2 table)
| SUB (e1, e2) -> (galculator_var e1 table) -. (galculator_var e2 table)
| MUL (e1, e2) -> (galculator_var e1 table) *. (galculator_var e2 table)
| DIV (e1, e2) -> (galculator_var e1 table) /. (galculator_var e2 table)
| SIGMA (e1, e2, e3) ->
	let i1 = (Pervasives.int_of_float (galculator_var e1 table)) in
	let i2 = (Pervasives.int_of_float (galculator_var e2 table)) in
	let rec sigma i1 i2 e =
	if i1 > i2 then 0.0
			   else (galculator_var e ((Pervasives.float_of_int i1)::table)) +. (sigma (i1 + 1) i2 e) in
	sigma i1 i2 e3
| INTEGRAL (e1, e2, e3) ->
	let f1 = (galculator_var e1 table) in
	let f2 = (galculator_var e2 table) in
	let rec riemann_sum f1 f2 e =
	if f1 > f2 then 0.0 -. (riemann_sum f2 f1 e)
			else if f1 +. 0.1 > f2 then 0.0
			          else (galculator_var e (f1::table)) *. 0.1 +. (riemann_sum (f1 +. 0.1) f2 e) in
	riemann_sum f1 f2 e3
in
(galculator_var e [])
