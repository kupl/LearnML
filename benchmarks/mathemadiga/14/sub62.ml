type exp = X
        |INT of int
        |REAL of float
        |ADD of exp * exp
        |SUB of exp * exp
        |MUL of exp * exp
        |DIV of exp * exp
        |SIGMA of exp * exp * exp
        |INTEGRAL of exp * exp * exp

exception FreeVariable
(* 이 exception을 어떻게 내는가?? *)


(* stack overflow 발생 *)
(* tail recursion 으로... *)
let rec galculator arg =
    
    let sumcal (p,q,r) =
        let rec sumsubcal (a,b,ex) sum =
            if a <= b
                then sumsubcal (a+.1.0,b,ex) (sum +.(ex a))
        else sum
    in if p > q then 0.0 else sumsubcal (float_of_int(p),float_of_int(q),r) 0.0 in

    let incal (p,q,r) =
        let rec insubcal (a,b,ex) sum =
            if a < b
                then insubcal (a+.0.1,b,ex) (sum +.(ex a))
        else sum *. 0.1
    in if p > q then (-1.0) *. (insubcal (q,p,r) 0.0) else insubcal (p,q,r) 0.0 in

    let rec subgal ar =
        match ar with
            |X -> (fun x -> x)
            |INT a -> (fun x -> (float_of_int a))
            |REAL a -> (fun x -> a)
            |ADD (a,b) -> (fun x -> ((subgal a) x) +. ((subgal b) x))
            |SUB (a,b) -> (fun x -> ((subgal a) x) -. ((subgal b) x))
            |MUL (a,b) -> (fun x -> ((subgal a) x) *. ((subgal b) x))
            |DIV (a,b) -> (fun x -> ((subgal a) x) /. ((subgal b) x))
            |SIGMA (a,b,c) -> (fun x -> sumcal (int_of_float((subgal a) x), int_of_float((subgal b) x), (subgal c)))
            |INTEGRAL (a,b,c) -> (fun x -> incal ((subgal a) x, (subgal b) x, (subgal c))) in
    match arg with
        |X -> raise FreeVariable
        |INT a -> float_of_int a
        |REAL a -> a
        |ADD (a,b) -> (galculator a) +. (galculator b)
        |SUB (a,b) -> (galculator a) -. (galculator b)
        |MUL (a,b) -> (galculator a) *. (galculator b)
        |DIV (a,b) -> (galculator a) /. (galculator b)
        |SIGMA (a,b,c) -> sumcal(int_of_float(galculator a),int_of_float(galculator b),subgal c)
        |INTEGRAL (a,b,c) -> incal(galculator a,galculator b,subgal c)
