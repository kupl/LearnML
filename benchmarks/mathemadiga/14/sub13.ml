exception FreeVariable

type exp = X
         | INT of int
         | REAL of float
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp
         | INTEGRAL of exp * exp * exp

type bind = UNBINDED
          | BINDED of exp

let rec bind_galculator: exp * bind -> float =
  fun (e, b) ->
    let rec sigma_aux: int * int * exp * float -> float =
      fun (fromX, toX, e, result) ->
        if fromX > toX then result
        else
          let new_result = result +. bind_galculator (e, BINDED (INT fromX)) in
          sigma_aux (fromX + 1, toX, e, new_result) in
    let rec integral_aux: float * float * exp * float -> float =
      fun (fromX, toX, e, result) ->
        if toX -. fromX < 0.1 then result
        else
          let new_result = result +. 0.1 *. bind_galculator (e, BINDED (REAL fromX)) in
          integral_aux (fromX +. 0.1, toX, e, new_result) in
    match (e, b) with
    | (X, UNBINDED) -> raise FreeVariable
    | (X, BINDED e) -> bind_galculator (e, UNBINDED)
    | (INT i, _) -> float_of_int i
    | (REAL f, _) -> f
    | (ADD (e1, e2), _) -> bind_galculator (e1, b) +. bind_galculator (e2, b)
    | (SUB (e1, e2), _) -> bind_galculator (e1, b) -. bind_galculator (e2, b)
    | (MUL (e1, e2), _) -> bind_galculator (e1, b) *. bind_galculator (e2, b)
    | (DIV (e1, e2), _) -> bind_galculator (e1, b) /. bind_galculator (e2, b)
    | (SIGMA (e1, e2, e3), _) ->
        let i1 = int_of_float (bind_galculator (e1, b)) in
        let i2 = int_of_float (bind_galculator (e2, b)) in
        sigma_aux (i1, i2, e3, 0.0)
    | (INTEGRAL (e1, e2, e3), _) ->
        let r1 = bind_galculator (e1, b) in
        let r2 = bind_galculator (e2, b) in
        if r1 > r2 then (-. integral_aux(r2, r1, e3, 0.0))
        else integral_aux (r1, r2, e3, 0.0)

let galculator: exp -> float =
  fun e ->
   bind_galculator (e, UNBINDED) 
