(* 4190,310 Programming Language (Fall 2014)
 * Homework 2 - Exercise 4
 * CSE / 2012-13456 / Gao, Chengbin *)

exception FreeVariable

type exp = X
         | INT of int
         | REAL of float
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp
         | INTEGRAL of exp * exp * exp

type value = D of float | UD 

let galculator e =
    let rec eval e x =
        match e with
        | X -> (match x with UD -> raise FreeVariable | D f -> f)
        | INT(i) -> float_of_int i
        | REAL(f) -> f
        | ADD(e1, e2) -> eval e1 x +. eval e2 x
        | SUB(e1, e2) -> eval e1 x -. eval e2 x
        | MUL(e1, e2) -> eval e1 x *. eval e2 x
        | DIV(e1, e2) -> eval e1 x /. eval e2 x
        | SIGMA(el, eh, ev) ->
                let l = float_of_int (int_of_float (eval el x)) in
                let h = float_of_int (int_of_float (eval eh x)) in
                sigma ev l h 1.0
        | INTEGRAL(el, eh, ev) -> 
                let l = eval el x in
                let h = eval eh x in
                if abs_float (h -. l) < 0.1 then 0.0 else
                    if l < h 
                    then sigma ev l h 0.1  
                    else -. (sigma ev h l 0.1)
    and sigma e l h step =
        if l > h || (step = 0.1 && l +. 0.1 > h)
        then 0.0
        else (eval e (D l)) *. step  +. (sigma e (l +. step) h step)
    in eval e UD

