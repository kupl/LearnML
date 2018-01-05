exception NOTAVALUE
exception FLOATSIGMA

type exp = X
| INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp

let float_to_exp a =
    REAL a


let rec convertX ((v:float),(e:exp)) =
    match e with
    | X -> v
    | INT a -> float_of_int a
    | REAL b -> b
    | ADD (a,b) -> convertX(v,a) +. convertX(v,b)
    | SUB (a,b) -> convertX(v,a) -. convertX(v,b)
    | MUL (a,b) -> convertX(v,a) *. convertX(v,b)
    | DIV (a,b) -> convertX(v,a) /. convertX(v,b)
    | SIGMA (a,b,c) ->
            sigma(float_to_exp(convertX(v,a)),float_to_exp(convertX(v,b)),c)
    | INTEGRAL (a,b,c) -> integral(float_to_exp(convertX(v,a)),
    float_to_exp(convertX(v,b)), c)

and sigma ((a:exp) ,(b:exp) , (c:exp)) =
    if (mathemadiga(a) -. 0.00001 > float_of_int(truncate(mathemadiga(a) -.
    0.0001)) +. 0.999)
    && (mathemadiga(b) -. 0.00001 > float_of_int(truncate(mathemadiga(b) -.
    0.0001)) +. 0.999) 
    then
        if mathemadiga(a) = mathemadiga(b) then
            convertX(mathemadiga(a),c)
        else
            convertX(mathemadiga(a),c) +. sigma(float_to_exp(mathemadiga(a) +.
            1.0),b,c)
    else raise FLOATSIGMA

    and integral ((a:exp),(b:exp),(c:exp)) =
        if (mathemadiga(b) -.
        mathemadiga(a)) > 0.01 then
            (convertX(mathemadiga(a),c) *. 0.1) +.
            integral(float_to_exp(mathemadiga(a) +. 0.1), b,c)
    else
        0.0

    and mathemadiga (exp) =
        match exp with
        | X -> raise NOTAVALUE
        | INT a -> float_of_int a
        | REAL a -> a
        | ADD (a,b) -> mathemadiga(a) +. mathemadiga(b)
        | SUB (a,b) -> mathemadiga(a) -. mathemadiga(b)
        | MUL (a,b) -> mathemadiga(a) *. mathemadiga(b)
        | DIV (a,b) -> mathemadiga(a) /. mathemadiga(b)
        | SIGMA (a,b,c) -> sigma(a,b,c)
        | INTEGRAL (a,b,c) -> integral(a,b,c)








