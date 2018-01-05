type exp = X
| INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp

let rec assign exp x=(match exp with
        X->REAL(x)
        |INT(e)->INT(e)
        |REAL(e)->REAL(e)
        |ADD(e1,e2)->ADD((assign e1 x),(assign e2 x))
        |SUB(e1,e2)->SUB((assign e1 x),(assign e2 x))
        |MUL(e1,e2)->MUL((assign e1 x),(assign e2 x))
        |DIV(e1,e2)->DIV((assign e1 x),(assign e2 x))
        |SIGMA(e1,e2,e3)->SIGMA((assign e1 x),(assign e2 x),(assign e3 x))
        |INTEGRAL(e1,e2,e3)->INTEGRAL((assign e1 x),(assign e2 x),(assign e3 x))
        )

let rec mathemadiga exp=
        (match exp with
        X->raise (Invalid_argument "mathemadiga: can't calculate")
        |INT(x)->float_of_int(x)
        |REAL(x)->x
        |ADD(x,y)->mathemadiga(x)+.mathemadiga(y)
        |SUB(x,y)->mathemadiga(x)-.mathemadiga(y)
        |MUL(x,y)->mathemadiga(x)*.mathemadiga(y)
        |DIV(x,y)->mathemadiga(x)/.mathemadiga(y)
        |SIGMA(x,n,f)->if (mathemadiga(assign n
        (mathemadiga(x))))>=mathemadiga(x)+.1.0 then mathemadiga(assign
        f (mathemadiga(x)))+.mathemadiga(SIGMA(ADD(x,INT(1)),n,f)) 
                else if (mathemadiga(assign n
        (mathemadiga(x))))>=mathemadiga(x) then mathemadiga(assign
        f (mathemadiga(x)))
                else raise (Invalid_argument "sigma")
        |INTEGRAL(x,n,f)->if ((mathemadiga(assign n
        (mathemadiga(x))))-.0.1)>mathemadiga(x) then
                mathemadiga(assign f
                (mathemadiga(x)))*.0.1+.mathemadiga(INTEGRAL(ADD(x,REAL(0.1)),n,f))
        else if (mathemadiga(assign n (mathemadiga(x))))>mathemadiga(x) then
                mathemadiga(assign f (mathemadiga(x)))*.0.1
        else if ((mathemadiga(assign n
        (mathemadiga(x))))+.0.1)<mathemadiga(x) then
                mathemadiga(assign f
                (mathemadiga(x)))*.(-0.1)+.mathemadiga(INTEGRAL(SUB(x,REAL(0.1)),n,f))
        else if (mathemadiga(assign n (mathemadiga(x))))<mathemadiga(x) then
                mathemadiga(assign f (mathemadiga(x)))*.(-0.1)
        else 0.
        )

(* TEST CODE
let f=ADD(X,INT 1)


mathemadiga(INTEGRAL(INT 2,INT 3,f))
mathemadiga(SIGMA(INT 2,INT 3,f))
*)
