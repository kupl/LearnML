exception InvalidSigma
exception FreeVariable

type exp = X
        | INT of int
        | REAL of float
        | ADD of exp * exp
        | SUB of exp * exp
        | MUL of exp * exp
        | DIV of exp * exp
        | SIGMA of exp * exp * exp
        | INTEGRAL of exp * exp * exp;;

let rec sigma_oper ((n:float), ex) =
        match ex with
        | X -> n
        | INT a -> float_of_int a
        | REAL a -> a
        | ADD (a,b) -> (sigma_oper (n,a)) +. (sigma_oper (n,b))
        | SUB (a,b) -> (sigma_oper (n,a)) -. (sigma_oper (n,b))
        | MUL (a,b) -> (sigma_oper (n,a)) *. (sigma_oper (n,b))
        | DIV (a,b) -> (sigma_oper (n,a)) /. (sigma_oper (n,b))
        | SIGMA ( a, b,c) -> (sigma_result (a , b , c))
        | INTEGRAL (a,b,c) -> (integral_result ((sigma_oper (n,a)), (sigma_oper (n, b)), c))
and sigma_result (a, b, (c:exp)) =
        match (a,b,c) with
        (INT i, INT j, k) -> if a > b then (float_of_int 0)
                                else (sigma_oper ((float_of_int i), k)) +. (sigma_result (INT (i+1), INT j, k))
        | _ -> raise InvalidSigma;
and integral_result (a, b, c) =
        if a >= (b -. 0.099999) then (float_of_int 0)
        else ((sigma_oper (a, c)) *. 0.1) +. (integral_result (a +. 0.1, b, c));;


let rec mathemadiga ex =
        match ex with
        | X -> raise FreeVariable
        | INT a -> float_of_int a
        | REAL a -> a
        | ADD (a,b) -> (mathemadiga a) +. (mathemadiga b)
        | SUB (a,b) -> (mathemadiga a) -. (mathemadiga b)
        | MUL (a,b) -> (mathemadiga a) *. (mathemadiga b)
        | DIV (a,b) -> (mathemadiga a) /. (mathemadiga b)
        | SIGMA ( a, b,c) -> (sigma_result (INT (int_of_float (mathemadiga a)) ,INT (int_of_float (mathemadiga b)) , c))
        | INTEGRAL (a,b,c) -> (integral_result ((mathemadiga a), (mathemadiga b), c));;


