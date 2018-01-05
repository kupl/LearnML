
type exp =
  | X
  | INT of int
  | REAL of float
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
  | INTEGRAL of exp * exp * exp
type var =
  | NONE
  | SOME of float

exception FreeVariable

let galculator : exp -> float =
  fun expr ->
    let rec galculatorRec : exp -> var -> float =
      fun expr v ->
        match expr with
        | X ->
            (match v with
            | NONE -> raise FreeVariable
            | SOME r -> r)
        | INT n -> float_of_int n
        | REAL r -> r
        | ADD (e1, e2) -> (galculatorRec e1 v) +. (galculatorRec e2 v)
        | SUB (e1, e2) -> (galculatorRec e1 v) -. (galculatorRec e2 v)
        | MUL (e1, e2) -> (galculatorRec e1 v) *. (galculatorRec e2 v)
        | DIV (e1, e2) -> (galculatorRec e1 v) /. (galculatorRec e2 v)
        | SIGMA (a, b, e) ->
            let valA = int_of_float (galculatorRec a v)
            and valB = int_of_float (galculatorRec b v)
            in if valA > valB then 0.0
            else
              (galculatorRec e (SOME (float_of_int valA))) +.
              (galculatorRec (SIGMA (INT (valA + 1), INT valB, e)) v)
        | INTEGRAL (a, b, e) ->
            let valA = galculatorRec a v in
            let valB = galculatorRec b v in
            let exit_condition = valA -. valB < 0.1 && valB -. valA < 0.1
            in if exit_condition then 0.0
            else if valA > valB then
              0.0 -. (galculatorRec (INTEGRAL (REAL valB, REAL valA, e)) v)
            else
              0.1 *.  (galculatorRec e (SOME valA)) +.
              (galculatorRec (INTEGRAL (REAL (valA +. 0.1), REAL valB, e)) v)
    in galculatorRec expr NONE

