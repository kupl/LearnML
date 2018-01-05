type exp = X
  | INT of int
  | REAL of float
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
  | INTEGRAL of exp * exp * exp

exception FreevarError
exception DividedByZero 

let mathemadiga : exp -> float =
  fun exp ->
    let rec eval : exp -> bool -> float -> float =
      fun exp -> fun b -> fun x -> match exp
      with X -> if b then x else raise FreevarError
        | INT(i) -> float_of_int(i)
        | REAL(f) -> f
        | ADD(e1,e2) -> (eval e1 b x) +. (eval e2 b x)
        | SUB(e1,e2) -> (eval e1 b x) -. (eval e2 b x)
        | MUL(e1,e2) -> (eval e1 b x) *. (eval e2 b x)
        | DIV(e1,e2) -> if (eval e2 b x)=0.0 then raise DividedByZero else (eval e1 b x) /. (eval e2 b x)
        | SIGMA(e1,e2,e3) ->
            let rec sigma : float -> float -> exp -> float =
              fun x -> fun max -> fun exp ->
                if x > max then 0.0 else (sigma (x+.1.0) max exp) +. (eval exp true x)
            in sigma (eval e1 b x) (eval e2 b x) e3
        | INTEGRAL(e1,e2,e3) ->
            let rec integral : float -> float -> exp -> float =
              fun x -> fun max -> fun exp ->
                let interval = if max-.x >= 0.1 then 0.1 else max -. x in
                  if interval <= 0.0 then 0.0 else (integral (x+.interval) max exp) +. ((eval exp true x)*.interval)
            in
              if (eval e1 b x) < (eval e2 b x) then integral (eval e1 b x) (eval e2 b x) e3 else 0.0-.(integral (eval e2 b x) (eval e1 b x) e3)
    in
      eval exp false 0.0
