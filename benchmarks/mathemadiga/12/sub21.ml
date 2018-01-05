type exp = X
| INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp

exception Error

let rec mathemadiga e = 
  let rec putreal(e, x) =  (* X 대신에 x 를 넣어준다 *)
    match e with 
    |X -> REAL(x)
    |INT(_) -> e
    |REAL(_) -> e
    |ADD(a,b) -> ADD(putreal(a,x), putreal(b,x))
    |SUB(a,b) -> SUB(putreal(a,x), putreal(b,x))
    |MUL(a,b) -> MUL(putreal(a,x), putreal(b,x))
    |DIV(a,b) -> DIV(putreal(a,x), putreal(b,x))
    |SIGMA(a,b,c) -> SIGMA(putreal(a,x), putreal(b,x), c)
    |INTEGRAL(a,b,c) -> INTEGRAL(putreal(a,x), putreal(b,x), c)
  in

  match e with
  | X -> raise Error
  | INT(a) -> float_of_int(a)
  | REAL(a) -> a
  | ADD(a, b) -> mathemadiga(a) +. mathemadiga(b)
  | SUB(a, b) -> mathemadiga(a) -. mathemadiga(b)
  | MUL(a, b) -> mathemadiga(a) *. mathemadiga(b)
  | DIV(a, b) -> mathemadiga(a) /. mathemadiga(b)
  | SIGMA(down, up, e) -> (
    match (down, up) with
    | (INT(a), INT(b)) -> (
      if a > b then
        0.0
      else
        mathemadiga(putreal(e, float_of_int a)) +. 
        mathemadiga(SIGMA(INT(a+1), up, e))
    )
    | (REAL(a), _) -> mathemadiga(SIGMA(INT(int_of_float a), up, e))
    | (_, REAL(b)) -> mathemadiga(SIGMA(down, INT(int_of_float b), e))
    | _ -> raise Error
  )
  | INTEGRAL(down, up, e) -> (
    if mathemadiga(down) > mathemadiga(up) then
      0.0 -. mathemadiga(INTEGRAL(up, down, e))
    else if mathemadiga(up) -. mathemadiga(down) < 0.1 then
      (mathemadiga(up) -. mathemadiga(down)) *. mathemadiga(putreal(e, mathemadiga(down)))
    else
      0.1 *. mathemadiga(putreal(e, mathemadiga(down))) +.
      mathemadiga(INTEGRAL(REAL(mathemadiga(down) +. 0.1), up, e))
  )
