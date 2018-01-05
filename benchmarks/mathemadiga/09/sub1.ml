type exp = X
             | INT of int
             | REAL of float
             | ADD of exp * exp
             | SUB of exp * exp
             | MUL of exp * exp
             | DIV of exp * exp
             | SIGMA of exp * exp * exp
             | INTEGRAL of exp * exp * exp

exception InvalidSigma
exception FreeVariable
exception DivideByZero

let rec mathemadiga (e : exp) =

 let rec sigma(a,b,f) =
           if (b -. a) < 0.0 then 0.0
           else f(a) +. sigma((a +. 1.0),b,f) in

 let rec integral(a,b,f) =
     let dx = 0.1 in
        if a>b then integral(b,a,f) *. -1.0
        else if (b -. a) = dx then f(a) *. dx
        else if (b -. a) < dx then f(a) *. (b -. a)
        else (f(a) *. dx) +. integral((a +. dx), b, f) in

 let rec makefun(e : exp) =  fun x ->
    let rec eval(e) =  match e with
               X -> x
            |  INT a -> float a
            | REAL a -> a
            | ADD(a,b) -> eval(a) +. eval(b)
            | SUB(a,b) -> eval(a) -. eval(b)
            | MUL(a,b) -> eval(a) *. eval(b)
            | DIV(a,b) -> if (eval(b)=0.0) then raise DivideByZero
                          else eval(a) /. eval(b)
            | SIGMA(a,b,c) -> if eval(a) > eval(b) then raise InvalidSigma
                                      else sigma( eval(a), eval(b), makefun(c))
            | INTEGRAL(a,b,c) -> integral( eval(a), eval(b), makefun(c))   in

    eval(e) in

 match e with
           X -> raise FreeVariable  
       |  INT a -> float a
       | REAL a -> a
       | ADD(a,b) -> mathemadiga(a) +. mathemadiga(b)
       | SUB(a,b) -> mathemadiga(a) -. mathemadiga(b)
       | MUL(a,b) -> mathemadiga(a) *. mathemadiga(b)
       | DIV(a,b) -> if (mathemadiga(b)=0.0) then raise DivideByZero
                          else mathemadiga(a) /. mathemadiga(b)
       | SIGMA(a,b,c) -> if mathemadiga(a) > mathemadiga(b) then raise InvalidSigma
                                 else sigma( mathemadiga(a), mathemadiga(b), makefun(c))
       | INTEGRAL(a,b,c) -> integral( mathemadiga(a), mathemadiga(b), makefun(c));;