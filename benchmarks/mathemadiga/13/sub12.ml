type exp = X
         | INT of int
         | REAL of float
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp
         | INTEGRAL of exp * exp * exp

type env = E of float
         | N_A 

exception FreeVariable

let rec galculatorT e (n: env) (* Environment *) = 
    match e with
    | X -> 
    begin 
        match n with
        | N_A -> raise FreeVariable
        | E x -> x
    end
    | INT i -> float_of_int i
    | REAL f -> f
    | ADD (e1, e2) -> (galculatorT e1 n) +. (galculatorT e2 n)
    | SUB (e1, e2) -> (galculatorT e1 n) -. (galculatorT e2 n)
    | MUL (e1, e2) -> (galculatorT e1 n) *. (galculatorT e2 n)
    | DIV (e1, e2) -> (galculatorT e1 n) /. (galculatorT e2 n)
    | SIGMA (e1, e2, e3) ->
    begin
        let start = galculatorT e1 n in
        let fin = galculatorT e2 n in
        let startN = int_of_float start in
        let finN = int_of_float fin in
        let rec loop sum i = 
            if i <= finN then let t = (galculatorT e3 (E (float_of_int i))) in
            loop (sum +. t) (i + 1) 
            else sum in
        loop 0.0 startN
    end
    | INTEGRAL (e1, e2, e3) -> 
    begin
        let start = galculatorT e1 n in
        let fin = galculatorT e2 n in
        let rec loop sum s f = 
            if (s +. 0.1) >= f then sum
            else let v = galculatorT e3 (E s) in 
            loop (v *. 0.1 +. sum) (s +. 0.1) f in
        if start < fin then loop 0.0 start fin
        else -. (loop 0.0 fin start)
    end
let galculator e = galculatorT e N_A

