type exp =
    X
    | INT of int
    | REAL of float
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | SIGMA of exp * exp * exp
    | INTEGRAL of exp * exp * exp
;;

exception FreeVariable

type var =
    UNDEFINED
    | R of float
;;

let rec my_calc e x = match (e,x) with
    (REAL(r),_) -> r
    | (INT(i),_) -> float_of_int i
    | (ADD(e1,e2),_) -> (my_calc e1 x)+.(my_calc e2 x)
    | (SUB(e1,e2),_) -> (my_calc e1 x)-.(my_calc e2 x)
    | (MUL(e1,e2),_) -> (my_calc e1 x)*.(my_calc e2 x)
    | (DIV(e1,e2),_) -> (my_calc e1 x)/.(my_calc e2 x)
    | (SIGMA(e1,e2,e3),_) ->
            let lo = int_of_float (my_calc e1 x) in
            let hi = int_of_float (my_calc e2 x) in
            let rec my_sigma l h now ex v =
                let det = R(float_of_int now) in
                if(now>h) then v
                else begin
                    let v_new = v +. (my_calc ex det) in
                    my_sigma l h (now+1) ex v_new
                end
            in
            my_sigma lo hi lo e3 0.0
    | (INTEGRAL(e1,e2,e3),_) ->
            let lo = (my_calc e1 x) in
            let hi = (my_calc e2 x) in
            let rec my_integral l h now ex v =
                let det = R(now) in
                if(h < l) then 0. -. (my_integral h l h ex v)
                else if(h -. now < 0.1) then v
                else begin
                    let v_new = v +. (my_calc ex det) *. 0.1 in
                    my_integral l h (now+.0.1) ex v_new
                end
            in
            my_integral lo hi lo e3 0.0
    | (X,UNDEFINED) -> raise FreeVariable
    | (X,R(r)) -> r
;;

let galculator e = my_calc e UNDEFINED;;
