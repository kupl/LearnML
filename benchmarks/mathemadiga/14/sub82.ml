(*
 * Brief      : HW2, Program Language (4190.310)
 * Author     : YongKi Kim <kim.yongki@ropas.snu.ac.kr>
 * Student Id : 2014-21767
 * Date       : Sep. 23, 2014
 *)

(* Exercise 4 : Galculator *)

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

let galculator: exp -> float = fun exp ->
let rec galculator_sub = fun exp var -> match exp with
| INT n -> float_of_int n
| REAL f -> f
| ADD (e1,e2) -> (galculator_sub e1 var) +. (galculator_sub e2 var)
| SUB (e1,e2) -> (galculator_sub e1 var) -. (galculator_sub e2 var)
| MUL (e1,e2) -> (galculator_sub e1 var) *. (galculator_sub e2 var)
| DIV (e1,e2) -> (galculator_sub e1 var) /. (galculator_sub e2 var)
| X -> (match var with [] -> raise FreeVariable | None::_ -> raise FreeVariable | (Some v)::_ -> v)
| SIGMA (e1,e2,e3) ->
	let v1 = (galculator_sub e1 var) in
	let v2 = (galculator_sub e2 var) in
        let rec sigma = fun (a, b, exp, var) ->
          if a > b then 0.
          else (galculator_sub exp ((Some b)::var)) +. sigma (a, b-.1., exp, var)
        in
          sigma (v1, v2, e3, var)
| INTEGRAL (e1,e2,e3) ->
	let v1 = (galculator_sub e1 var) in
	let v2 = (galculator_sub e2 var) in
        let rec integral = fun (a, b, exp, var) ->
	  let diff = b -. a in
	  if diff < 0.1 then 0.
          else (0.1*.galculator_sub exp ((Some a)::var)) +. integral (a+.0.1, b, exp, var)
        in
	  if v1 = v2 then 0.
          else if v1 > v2 then -.integral (v2, v1, e3, var)
          else integral (v1, v2, e3, var)
in
  (galculator_sub exp [])