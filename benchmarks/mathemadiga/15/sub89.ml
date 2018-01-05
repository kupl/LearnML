type exp = X | INT of int | REAL of float
        | ADD of exp * exp | SUB of exp * exp
        | MUL of exp * exp | DIV of exp * exp
        | SIGMA of exp * exp * exp | INTEGRAL of exp * exp * exp
let rec func : exp->(float->float)= fun exp ->
        match exp with
        | X -> let f x=x in f
        | INT(i) -> let f x=float_of_int i in f
        | REAL(i) -> let f x=i in f
        | ADD(e1,e2) ->
                let f:float->float=fun x->(func(e1) x)+.(func(e2) x) in f
        | SUB(e1,e2) ->
                let f:float->float=fun x->(func(e1) x)-.(func(e2) x) in f
        | MUL(e1,e2) ->
                let f:float->float=fun x->(func(e1) x)*.(func(e2) x) in f
        | DIV(e1,e2) ->
                let f:float->float=fun x->(func(e1) x)/.(func(e2) x) in f
        | SIGMA(_,_,_) | INTEGRAL(_,_,_) ->
                let f:float->float=fun x->galculator(exp) in f
and galculator : exp -> float = fun e ->
        match e with
        | X -> 0.0
        | INT(i) -> float_of_int i
        | REAL(i) -> i
        | ADD(e1,e2) -> galculator(e1)+.galculator(e2)
        | SUB(e1,e2) -> galculator(e1)-.galculator(e2)
        | MUL(e1,e2) -> galculator(e1)*.galculator(e2)
        | DIV(e1,e2) -> galculator(e1)/.galculator(e2)
        | SIGMA(e1,e2,e3) ->
                let a = galculator(e1) in
                let b = galculator(e2) in
                let f = func(e3) in
                sigma(a,b,f,0.0)
        | INTEGRAL(e1,e2,e3) ->
                let a = galculator(e1) in
                let b = galculator(e2) in
                let f = func(e3) in
                if (a>b) then 0.0-.integral(b,a,f,0.0)
		else integral(a,b,f,0.0)
and sigma : (float*float*(float->float)*float) -> float = fun (a,b,f,x) ->
        if (a>b) then x
        else sigma(a,b-.1.0,f,x+.f(b))
and integral : (float*float*(float->float)*float) -> float = fun (a,b,f,x) ->
        if (a>b) then 0.0-.integral(b,a,f,x)
        else if b-.a<0.1 then x
        else integral(a+.0.1,b,f,x+.(f(a)*.0.1))

(*
let f1 = galculator (INTEGRAL(INT 0, INT 1000, DIV (SUB(MUL(INT 16, DIV(X, INT 1000)), INT 16),ADD(SUB(MUL(MUL(DIV(X, INT 1000),DIV(X, INT 1000)),MUL(DIV(X, INT 1000),DIV(X, INT 1000))), MUL(MUL(INT 2,DIV(X, INT 1000)),MUL(DIV(X, INT 1000),DIV(X, INT 1000)))) , SUB(MUL(INT 4, DIV(X, INT 1000)), INT 4) ) ) ))
let f2 = galculator(SIGMA(INT 1, INT 1000, DIV(INT 8, MUL(SUB(MUL(INT 2, X), INT 1),SUB(MUL(INT 2, X), INT 1)))))
let _ = print_endline(string_of_float f1)
let _ = print_endline(string_of_float f2)
let f1=galculator (INTEGRAL(INT 0, INT 100000, DIV (SUB(MUL(INT 16, DIV(X, INT 100000)), INT 16),ADD(SUB(MUL(MUL(DIV(X, INT 100000),DIV(X, INT 100000)),MUL(DIV(X, INT 100000),DIV(X, INT 100000))), MUL(MUL(INT 2,DIV(X, INT 100000)),MUL(DIV(X, INT 100000),DIV(X, INT 100000)))) , SUB(MUL(INT 4, DIV(X, INT 100000)), INT 4) ) ) )) 
let f2=galculator(SIGMA(INT 1, INT 1000000, DIV(INT 8, MUL(SUB(MUL(INT 2, X), INT 1),SUB(MUL(INT 2, X), INT 1))))) 
let _ = print_endline(string_of_float f1)
let _ = print_endline(string_of_float f2)


let f1=galculator (SIGMA (INT 1, INT 10, SUB(MUL(X, X), INT 1)))
let f2=galculator (SIGMA (REAL 1., REAL 10., SUB(MUL(X, X), INT 1)))
let f3=galculator (INTEGRAL (REAL 1., REAL 10., SUB(MUL(X, X), INT 1))) 
let f4=galculator (INTEGRAL (REAL 10., REAL 1., SUB(MUL(X, X), INT 1))) 
let f5=galculator (INTEGRAL (REAL 10., SIGMA (REAL 1., REAL 10., SUB(MUL(X, X), INT 1)), SUB(MUL(X, X), INT 1))) 
let f6=galculator (INTEGRAL (REAL 10., SIGMA (REAL 1., REAL 10., SUB(MUL(X, X), INT 1)), SUB(MUL(X, X), X)))

let _ = print_endline(string_of_float f1)
let _ = print_endline(string_of_float f2)
let _ = print_endline(string_of_float f3)
let _ = print_endline(string_of_float f4)
let _ = print_endline(string_of_float f5)
let _ = print_endline(string_of_float f6)
*)
