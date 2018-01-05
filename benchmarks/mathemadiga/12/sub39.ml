exception InvalidSigma
exception FreeVariable

type exp = X
        | INT of int
        | REAL of float
        | ADD of exp * exp
        | SUB of exp * exp
        | MUL of exp * exp
        | DIV of exp * exp
        | SIGMA of exp * exp *exp
        | INTEGRAL of exp * exp * exp




let rec calsig (n, e) =

        let rec sigmacal (f1, f2, e) =
                if f1 > (f2 +. 0.5) then 0.
                else (calsig (f1, e)) +. (sigmacal ((f1 +. 1.0), f2, e)) in

        let rec integralcal2 (f1, f2, e) =
                if (f1 +. 0.1) > f2 then 0.
                else (0.1 *. (calsig (f1, e))) +. (integralcal2 ((f1 +. 0.1), f2, e)) in

        let rec integralcal1 (e1, e2, e) =
                if (calsig (n, e1) ) > (calsig (n, e2)) then (calsig (n, (SUB ((INT 0), (REAL (integralcal2 ((calsig (n, e1)), (calsig (n, e2)), e)))))))
                else (integralcal2 ((calsig (n, e1)), (calsig (n, e2)), e)) in


        match e with
        | X -> n
        | INT i -> float i
        | REAL r -> r
        | ADD (e1, e2) -> (calsig (n, e1)) +. (calsig (n, e2))
        | SUB (e1, e2) -> (calsig (n, e1)) -. (calsig (n, e2))
        | MUL (e1, e2) -> (calsig (n, e1)) *. (calsig (n, e2))
        | DIV (e1, e2) -> (calsig (n, e1)) /. (calsig (n, e2))
        | SIGMA ((INT n1), (INT n2), e) ->  if n1 > n2 then 0.
                                        else (sigmacal ((calsig (n, (INT n1))), (calsig (n, (INT n2))), e))
        | SIGMA (_, _, _) -> raise InvalidSigma
        | INTEGRAL (e1, e2, e) -> (integralcal1 (e1, e2, e))




let rec mathenatica e =

        let rec sigmacal (f1, f2, e) =
                if f1 > (f2 +. 0.5) then 0.
                else (calsig (f1, e)) +. (sigmacal ((f1 +. 1.0), f2, e)) in

        let rec integralcal2 (f1, f2, e) =
                if (f1 +. 0.1) > f2 then 0.
                else (0.1 *. (calsig (f1, e))) +. (integralcal2 ((f1 +. 0.1), f2, e)) in

        let rec integralcal1 (e1, e2, e) =
                if (mathenatica e1) > (mathenatica e2) then (mathenatica (SUB ((INT 0), (REAL (integralcal2 ((mathenatica e2), (mathenatica e1), e))))))
                else (integralcal2 ((mathenatica e1), (mathenatica e2), e)) in

        match e with
        | X -> raise FreeVariable
        | INT i -> float i
        | REAL r -> r
        | ADD (e1, e2) -> (mathenatica e1) +. (mathenatica e2)
        | SUB (e1, e2) -> (mathenatica e1) -. (mathenatica e2)
        | MUL (e1, e2) -> (mathenatica e1) *. (mathenatica e2)
        | DIV (e1, e2) -> (mathenatica e1) /. (mathenatica e2)
        | SIGMA ((INT n1), (INT n2), e) -> (sigmacal ((mathenatica (INT n1)), (mathenatica (INT n2)), e))
        | SIGMA (_, _, _) -> raise InvalidSigma
        | INTEGRAL (e1, e2, e) -> (integralcal1 (e1, e2, e))


