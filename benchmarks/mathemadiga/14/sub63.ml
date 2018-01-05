type exp =
    | X
    | INT of int
    | REAL of float
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | SIGMA of exp * exp * exp
    | INTEGRAL of exp * exp * exp;;

exception FreeVariable;;

let rec cal isFree curX exp =
    let calexp = cal isFree curX in
    match exp with
    | X -> if isFree then raise FreeVariable else curX
    | INT a -> float_of_int a
    | REAL a -> a
    | ADD(e1, e2) -> (calexp e1) +. (calexp e2)
    | SUB(e1, e2) -> (calexp e1) -. (calexp e2)
    | MUL(e1, e2) -> (calexp e1) *. (calexp e2)
    | DIV(e1, e2) -> (calexp e1) /. (calexp e2)
    | SIGMA(e1, e2, e3) ->
            let rec sigma left right exp sum =
                if left > right then sum
                else sigma (left+1) right exp (sum +. (cal false (float_of_int left) exp))
            in
            sigma (int_of_float (calexp e1)) (int_of_float (calexp e2)) e3 0.0
    | INTEGRAL(e1, e2, e3) ->
            let r1 = (calexp e1)
            and r2 = (calexp e2) in
            let rec integral left right exp sum =
                if right -. left < 0.1 then sum
                else integral (left +. 0.1) right exp (sum +. (cal false left exp) *. 0.1)
            in
            if r1<r2
            then integral r1 r2 e3 0.0
            else 0.0-.(integral r2 r1 e3 0.0);;


let galculator = cal true 0.0;;
