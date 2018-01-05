(* 4190.310 Programming Languages - Daegeun Lee <elnn@elnn.kr> *)

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

let rec galculator = fun e ->
    match e with
    | X -> raise FreeVariable
    | INT(n) -> float(n)
    | REAL(n) -> n
    | ADD(a,b) -> galculator(a) +. galculator(b)
    | SUB(a,b) -> galculator(a) -. galculator(b)
    | MUL(a,b) -> galculator(a) *. galculator(b)
    | DIV(a,b) -> galculator(a) /. galculator(b)  (* cf. Division_by_zero *)
    | SIGMA(st,en,y) -> sigma(int_of_float(galculator(st)),
                              int_of_float(galculator(en)),
                              y)
    | INTEGRAL(st,en,y) -> integral(galculator(st), galculator(en), y)

and eval = fun (e,x) ->
    match e with
    | X -> x
    | INT(n) -> float(n)
    | REAL(n) -> n
    | ADD(a,b) -> eval(a,x) +. eval(b,x)
    | SUB(a,b) -> eval(a,x) -. eval(b,x)
    | MUL(a,b) -> eval(a,x) *. eval(b,x)
    | DIV(a,b) -> eval(a,x) /. eval(b,x)  (* cf. Division_by_zero *)
    | SIGMA(st,en,y) -> sigma(int_of_float(eval(st,x)),
                              int_of_float(eval(en,x)),
                              y)
    | INTEGRAL(st,en,y) -> integral(eval(st,x),eval(en,x),y)

and sigma = fun(st,en,e) ->
    if st > en then 0.0
    else eval(e,float(st)) +. sigma(st+1,en,e)

and integral = fun(st,en,e) ->
    if st > en then -1.0 *. integral(en,st,e)
    else if st +. 0.1 > en then 0.0
    else (eval(e,st) *. 0.1) +. integral(st+.0.1,en,e)
