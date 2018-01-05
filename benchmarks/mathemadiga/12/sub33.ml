type exp = X
        | INT of int
        | REAL of float
        | SUB of exp * exp
        | ADD of exp * exp
        | MUL of exp * exp
        | DIV of exp * exp
        | SIGMA of exp * exp * exp
        | INTEGRAL of exp * exp * exp

exception FreeVariable
exception InvalidSigma

let rec mathemadiga exp =
        let rec sigma a b c =
                let makeint a =
                        match a with
                        | REAL b -> (INT (int_of_float b))
                        | _-> a
                in
                let rec calc exp2 x=
                        match (exp2,x) with
                        | (X, INT a) -> (float a)
                        | (X, REAL a) -> a
                        | (INT b,_) -> (float b)
                        | (REAL b,_) -> b
                        | (ADD (a,b),_) -> (calc a x) +. (calc b x)
                        | (SUB (a,b), _) -> (calc a x) -. (calc b x)
                        | (MUL (a,b), _) -> (calc a x) *. (calc b x)
                        | (DIV (a,b), _) -> (calc a x) /. (calc b x)
                        | (SIGMA (a,b,c), _) -> (sigma (REAL (calc a x)) (REAL (calc b x)) c)
                        | (INTEGRAL (a,b,c) ,_)-> (integral (REAL (calc a x)) (REAL (calc b x)) c)
                        | _-> raise FreeVariable
                in
                match (a,b,c) with
                | (INT m, INT n, exp) -> if(m<=n) then (calc exp a) +. (sigma (INT (m+1)) b c)
                                        else 0.0
                | (REAL m, INT n, exp) -> (sigma (makeint a) b exp)
                | (INT m, REAL n, exp) -> (sigma a (makeint b) exp)
                | (REAL m, REAL n, exp) -> (sigma (makeint a) (makeint b) exp)
                | _-> raise InvalidSigma
        and integral a b c =
                let rec calc exp2 x=
                        match (exp2,x) with
                        | (X, INT a) -> (float a)
                        | (X , REAL a) -> a
                        | (INT b,_) -> (float b)
                        | (REAL b,_) -> b
                        | (ADD (a,b),_) -> (calc a x) +. (calc b x)
                        | (SUB (a,b), _) -> (calc a x) -. (calc b x)
                        | (MUL (a,b), _) -> (calc a x) *. (calc b x)
                        | (DIV (a,b), _) -> (calc a x) /. (calc b x)
                        | (SIGMA (a,b,c), _) -> (sigma (REAL (calc a x)) (REAL (calc b x)) c)
                        | (INTEGRAL (a,b,c) ,_)-> (integral (REAL (calc a x)) (REAL (calc b x)) c)
                        | _-> raise FreeVariable
                in
                let rec makereal a =
                        match a with
                        | (INT a) -> (REAL (float a))
                        | _-> a
                        in
                match (a,b,c) with
                | (INT m, INT n, exp )-> (integral (makereal a) (makereal b) c)
                | (INT m, REAL n, exp) -> (integral (makereal a) (makereal b) c)
                | (REAL m, INT n, exp) -> (integral (makereal a) (makereal b) c)
                | (REAL m, REAL n, exp) -> if(m>n) then 0.0 -. (integral b a c)
                                                else if ( (m +. 0.1) >= n) then (n-.m) *. (calc c a)
                                                        else 0.1 *. (calc c a) +. (integral (REAL (m +. 0.1)) b c)
                |_-> raise FreeVariable

        in

        match exp with
        | INT a -> (float a)
        | REAL a -> a
        | ADD(a,b) -> (mathemadiga a) +. (mathemadiga b)
        | SUB(a,b) -> (mathemadiga a) -. (mathemadiga b)
        | MUL(a,b) -> (mathemadiga a) *. (mathemadiga b)
        | DIV(a,b) -> (mathemadiga a) /. (mathemadiga b)
        | SIGMA(a,b,c) -> (sigma a b c)
        | INTEGRAL(a,b,c) -> (integral a b c)
        | _-> raise FreeVariable

