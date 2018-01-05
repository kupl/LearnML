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

let rec galcX: exp -> float -> float =
    fun e x ->
        match e with
        | X -> x
        | INT a -> float_of_int a
        | REAL a -> a
        | ADD(a,b) -> (galcX a x) +. (galcX b x)
        | SUB(a,b) -> (galcX a x) -. (galcX b x)
        | MUL(a,b) -> (galcX a x) *. (galcX b x)
        | DIV(a,b) -> (galcX a x) /. (galcX b x)
        | SIGMA(a,b,f) ->
            let ga = (galcX a x) in
            let gb = (galcX b x) in
                (sigma (int_of_float ga) (int_of_float gb) f)
        | INTEGRAL(a,b,f) ->
            let ga = (galcX a x) in
            let gb = (galcX b x) in
                if ga > gb then -.(integral gb ga f)
                else (integral ga gb f)
        
and sigma: int -> int -> exp -> float =
    fun a b f ->
        if a > b then 0.
        else if a = b then
            (galcX f (float_of_int a))
        else
            (galcX f (float_of_int a)) +. (sigma (a+1) b f)

and integral: float -> float -> exp -> float =
    fun a b f ->
        if b-.a < 0.1 then 0.
        else (0.1 *. (galcX f a)) +. (integral (a +. 0.1) b f)


let rec galculator: exp -> float =
    fun e ->
        match e with
        | X -> raise FreeVariable
        | INT a -> float_of_int a
        | REAL a -> a
        | ADD(a,b) -> (galculator a) +. (galculator b)
        | SUB(a,b) -> (galculator a) -. (galculator b)
        | MUL(a,b) -> (galculator a) *. (galculator b)
        | DIV(a,b) -> (galculator a) /. (galculator b)
        | SIGMA(a,b,f) ->
            let ga = (galculator a) in
            let gb = (galculator b) in
                (sigma (int_of_float ga) (int_of_float gb) f)
        | INTEGRAL(a,b,f) ->
            let ga = (galculator a) in
            let gb = (galculator b) in
                if ga > gb then -.(integral gb ga f)
                else (integral ga gb f)


