exception FreeVariable;; 

type exp = X
        | INT of int
        | REAL of float
        | ADD of exp * exp  
        | SUB of exp * exp 
        | MUL of exp * exp
        | DIV of exp * exp
        | SIGMA of exp * exp * exp
        | INTEGRAL of exp * exp * exp;;

let rec subs : exp*exp->exp = fun (subsExp,tgt) -> 
    match tgt with
        X -> subsExp
        | INT i -> INT i
        | REAL f -> REAL f
        | ADD(e1, e2) -> ADD(subs(subsExp, e1),subs(subsExp, e2))
        | SUB(e1, e2) -> SUB(subs(subsExp, e1),subs(subsExp, e2))
        | MUL(e1, e2) -> MUL(subs(subsExp, e1),subs(subsExp, e2))
        | DIV(e1, e2) -> DIV(subs(subsExp, e1),subs(subsExp, e2))
        | SIGMA(e1,e2,e3) -> SIGMA(subs(subsExp, e1),subs(subsExp, e2),subs(subsExp,e3))
        | INTEGRAL(e1,e2,e3) -> INTEGRAL(subs(subsExp, e1),subs(subsExp, e2),subs(subsExp,e3));;
       
let rec gee : exp -> exp = fun e -> REAL(galculator(e))
and galculator : exp -> float = fun e -> 
match e with
    X -> raise FreeVariable
    | INT i -> float_of_int i
    | REAL f -> f
    | ADD(e1,e2) -> (
        match (e1,e2) with
            (INT i1, INT i2) -> (float_of_int (i1 + i2))
            | (INT i1, REAL r2) -> (float_of_int i1) +. r2
            | (REAL r1, INT i2) -> r1 +. (float_of_int i2)
            | (REAL r1, REAL r2) -> r1 +. r2 
            | (e1, e2) -> (galculator e1)+.(galculator e2)
    )
    | SUB(e1,e2) -> (
        match (e1,e2) with
            (INT i1, INT i2) -> (float_of_int (i1 - i2))
            | (INT i1, REAL r2) -> (float_of_int i1) -. r2
            | (REAL r1, INT i2) -> r1 -. (float_of_int i2)
            | (REAL r1, REAL r2) -> r1 -. r2 
            | (e1, e2) -> (galculator e1)-.(galculator e2)
    )
    | MUL(e1,e2) -> (
        match (e1,e2) with
            (INT i1, INT i2) -> (float_of_int (i1 * i2))
            | (INT i1, REAL r2) -> (float_of_int i1) *. r2
            | (REAL r1, INT i2) -> r1 *. (float_of_int i2)
            | (REAL r1, REAL r2) -> r1 *. r2 
            | (e1, e2) -> (galculator e1)*.(galculator e2)
    )
    | DIV(e1,e2) -> (
        match (e1,e2) with
            (INT i1, INT i2) -> (float_of_int (i1 / i2))
            | (INT i1, REAL r2) -> (float_of_int i1) /. r2
            | (REAL r1, INT i2) -> r1 /. (float_of_int i2)
            | (REAL r1, REAL r2) -> r1 /. r2 
            | (e1, e2) -> (galculator e1)/.(galculator e2)
    )
    | SIGMA(e1,e2,e3) -> (
        let ei1 = int_of_float (galculator (e1)) in
        let ei2 = int_of_float (galculator (e2)) in
        let ee1 = INT ei1 in
        let ee2 = INT ei2 in
        let nee1 = ADD(ee1, INT 1) in
            if(ee1>ee2) then (float_of_int 0) else (
                galculator(ADD(
                              subs(ee1,e3), 
                              SIGMA(nee1,ee2,e3)
                           ))
            )
    )
    | INTEGRAL(e1,e2,e3) -> (
        let ef1 = galculator(e1) in
        let ef2 = galculator(e2) in
        let dx = REAL 0.1 in
        let ee1 = REAL ef1 in
        let ee2 = REAL ef2 in
        let nee1 = ADD(ee1,dx) in
        if(ef1 > ef2) then (-1.*.galculator(INTEGRAL(e2,e1,e3)))
        else if (ef2-.ef1<=0.1) then (float_of_int 0)
        else ( 
            galculator(ADD(
                MUL(dx,subs(ee1, e3)),
                INTEGRAL(nee1,ee2,e3)
            ))
        )
    );;
         

(*
let print_ans = fun f ->
    print_float(f);
    print_endline "";;
*)
