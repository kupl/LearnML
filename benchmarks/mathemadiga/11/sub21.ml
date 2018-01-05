(* PL HW2-2, "계산기 mathemadiga"
   2007-11738
   알렉산더 *)

(* Type defenition *)
type exp = X
         | INT of int
         | REAL of float
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp
         | INTEGRAL of exp * exp * exp

exception InputError of string

(* Main Function *)
(* mathemadiga: exp -> float *)
let rec mathemadiga e =

    (* sigma function, sigmaFun: exp*exp*exp -> float *)
    let rec sigmaFun (numFrom, numTo, expres) =
        (* evolute one loop; sigmaOne: int*exp -> float *)
        let rec sigmaOne (x, sigmaExp) =
            match sigmaExp with
                X -> float_of_int x
              | INT i -> float_of_int i
              | REAL f -> f
              | ADD (e1, e2) -> (sigmaOne (x, e1)) +. (sigmaOne (x, e2))
              | SUB (e1, e2) -> (sigmaOne (x, e1)) -. (sigmaOne (x, e2))
              | MUL (e1, e2) -> (sigmaOne (x, e1)) *. (sigmaOne (x, e2))
              | DIV (e1, e2) -> (sigmaOne (x, e1)) /. (sigmaOne (x, e2))
              | SIGMA (n1, n2, eX) -> sigmaFun(n1, n2, eX)
              | INTEGRAL (n1, n2, eX) -> integralFun(n1, n2, eX)
        in
        (* evolute all loops from k to n; sigmaLoop: int*int -> float *)
        let rec sigmaLoop (k, n) =
            if  k > n then 0.0
            else sigmaOne (k, expres) +. (sigmaLoop ((k+1), n))
        in
        (* sigmaFun main *)
        match (numFrom, numTo) with
              (INT n1, INT n2) -> sigmaLoop (n1, n2)
            | (_, _) -> raise (InputError "First two argument for Sigma must be Integers")
    
    
    (* integral function; integralFun: exp*exp*exp -> float *)
    and integralFun (numFrom, numTo, expres) =
        (* evolute one loop; integralOne: float*exp -> float *)
        let rec integralOne (x, integralExp) =
            match integralExp with
                  X -> x
                | INT i -> float_of_int i
                | REAL f -> f
                | ADD (e1, e2) -> (integralOne (x, e1)) +. (integralOne (x, e2))
                | SUB (e1, e2) -> (integralOne (x, e1)) -. (integralOne (x, e2))
                | MUL (e1, e2) -> (integralOne (x, e1)) *. (integralOne (x, e2))
                | DIV (e1, e2) -> (integralOne (x, e1)) /. (integralOne (x, e2))
                | SIGMA (n1, n2, eX) -> sigmaFun(n1, n2, eX)
                | INTEGRAL (n1, n2, eX) -> integralFun(n1, n2, eX)
        in
        (* evolute all loops from k to n; integralLoop: float*float -> float *)
        let rec integralLoop (k, n) =
            if k >= n then 0.0
            else (integralOne(k, expres) *. 0.1) +. (integralLoop(k +. 0.1, n))
        in
        
        (* exp -> float *)
        let numFromFloat = mathemadiga numFrom in
        let numToFloat = mathemadiga numTo in
        
        (* evolute rest; rest: float*float -> float *)
        let rest =
            let rec restInner (m, n) =
                if (m < n) then restInner((m +. 0.1), n)
                else m -. n
            in
            if numFromFloat > numToFloat then restInner (numToFloat, numFromFloat)
            else restInner (numFromFloat, numToFloat)
        in
        
        (* integralFun main *)
        if (numFromFloat <= numToFloat)
        then (integralLoop (numFromFloat, (numToFloat -. rest))) +. 
             ((integralOne ((numToFloat -. rest), expres)) *. rest)
        else 0.0 -.((integralLoop (numToFloat, (numFromFloat -. rest))) +. 
                   ((integralOne ((numFromFloat -. rest), expres)) *. rest))
            
    in

    (* Main function *)
    match e with
        X -> raise (InputError "X is not in Sigma or Integral function")
      | INT i -> float_of_int i
      | REAL f -> f
      | ADD (e1, e2) -> (mathemadiga e1) +. (mathemadiga e2)
      | SUB (e1, e2) -> (mathemadiga e1) -. (mathemadiga e2)
      | MUL (e1, e2) -> (mathemadiga e1) *. (mathemadiga e2)
      | DIV (e1, e2) -> (mathemadiga e1) /. (mathemadiga e2)
      | SIGMA (n1, n2, eX) -> sigmaFun(n1, n2, eX)
      | INTEGRAL (n1, n2, eX) -> integralFun(n1, n2, eX)
