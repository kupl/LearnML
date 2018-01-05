(* hw2ex5.ml *)


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

let rec substitute (e:exp) (v: float) : float = 
  match e with
    | X -> v
    | INT i -> (float_of_int i)
    | REAL f -> f
    | ADD (e1,e2) -> (substitute e1 v) +. (substitute e2 v)
    | SUB (e1,e2) -> (substitute e1 v) -. (substitute e2 v)
    | MUL (e1,e2) -> (substitute e1 v) *. (substitute e2 v)
    | DIV (e1,e2) -> (substitute e1 v) /. (substitute e2 v)
    | SIGMA _ -> galculator e
    | INTEGRAL _ -> galculator e

and sigma (a: int) (b: int) (e: exp) (v: float) : float = 
  if a > b then 0.
  else if a = b then v +. (substitute e (float_of_int a))
  else sigma (a+1) (b) (e) (v +. (substitute e (float_of_int a)))


(* should fix stackoverflow in integral *)
and integral (a: float) (b: float) (e: exp) (v: float) : float = 
  if abs_float (a-.b) < 0.1 then v
  else if a > b then -.(integral b a e v)
  else (integral (a +. 0.1) b e (v +. (substitute e a)))

and galculator (e:exp) : float = 
  match e with
    | X -> raise FreeVariable
    | INT i -> (float_of_int i)
    | REAL f -> f
    | ADD (e1,e2) -> (galculator e1) +. (galculator e2)
    | SUB (e1,e2) -> (galculator e1) -. (galculator e2)
    | MUL (e1,e2) -> (galculator e1) *. (galculator e2)
    | DIV (e1,e2) -> (galculator e1) /. (galculator e2)
    | SIGMA (e1, e2, e3) ->
        (sigma
           (int_of_float (galculator e1))
           (int_of_float (galculator e2)) 
           e3
           0.
        )
    | INTEGRAL (e1, e2, e3) ->
        (integral
           (galculator e1)
           (galculator e2)
           e3
           0.
        ) *. 0.1


(* testcase

   let _ = galculator (INTEGRAL (REAL 10., SIGMA (REAL 1., REAL 10., SUB(MUL(X, X), INT 1)), SUB(MUL(X, X), INT 1)));;

   let _ = galculator (SIGMA (INT 0, INT 100000, MUL (X,X)))

   let _ = galculator (INTEGRAL(INT 1, INT 90, X))



   let _ = galculator (INTEGRAL(INT 0, INT 100000, DIV (SUB(MUL(INT 16, DIV(X, INT 100000)), INT 16),ADD(SUB(MUL(MUL(DIV(X, INT 100000),DIV(X, INT 100000)),MUL(DIV(X, INT 100000),DIV(X, INT 100000))), MUL(MUL(INT 2,DIV(X, INT 100000)),MUL(DIV(X, INT 100000),DIV(X, INT 100000)))) , SUB(MUL(INT 4, DIV(X, INT 100000)), INT 4) ) ) )) 


   let _ = galculator(SIGMA(INT 1, INT 1000000, DIV(INT 8, MUL(SUB(MUL(INT 2, X), INT 1),SUB(MUL(INT 2, X), INT 1)))))

*)
