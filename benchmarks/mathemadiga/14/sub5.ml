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

exception FreeVariable

let rec galculator : exp -> float =
  fun exp ->
    let rec galSub : exp * float list -> float =
      fun (exp, xList) ->
        match exp with
        | X -> if xList = [] 
                  then raise FreeVariable
               else List.hd xList
        | INT n -> float_of_int n
        | REAL f -> f
        | ADD (e1, e2) -> (galSub (e1, xList)) +. (galSub (e2, xList))
        | SUB (e1, e2) -> (galSub (e1, xList)) -. (galSub (e2, xList))
        | MUL (e1, e2) -> (galSub (e1, xList)) *. (galSub (e2, xList))
        | DIV (e1, e2) -> (galSub (e1, xList)) /. (galSub (e2, xList))
        | SIGMA (e1, e2, e3) -> 
          let s = int_of_float (galSub (e1, xList)) in
          let e = int_of_float (galSub (e2, xList)) in
          let rec sigmaSub : int * int * exp -> float =
            fun (s, e, exp) ->
              if s > e then 0.
              else if s = e then galSub (exp, [float_of_int s])
              else galSub (exp, [float_of_int s]) +. sigmaSub (s+1, e, exp)
          in
          sigmaSub (s, e, e3)
        | INTEGRAL (e1, e2, e3) ->
          let s = galSub (e1, xList) in
          let e = galSub (e2, xList) in
          let rec integralSub : float * float * exp -> float =
            fun (s, e, exp) ->
              if e -. s < 0.1 then 0.
              else galSub (exp, [s]) *. 0.1 +. integralSub (s+.0.1, e, exp)
          in
          if s > e then -.(integralSub (e, s, e3))
          else integralSub (s, e, e3)
    in
    galSub (exp, [])
