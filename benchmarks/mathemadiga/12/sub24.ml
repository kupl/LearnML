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

let rec mathemadiga e = 
  let rec sub(n,e) = 
    match e with
    | X -> n
    | INT i -> float_of_int i
    | REAL f -> f
    | ADD(e1,e2) -> sub(n,e1) +. sub(n,e2)
    | SUB(e1,e2) -> sub(n,e1) -. sub(n,e2)
    | MUL(e1,e2) -> sub(n,e1) *. sub(n,e2)
    | DIV(e1,e2) -> sub(n,e1) /. sub(n,e2)
    | SIGMA(e1,e2,e3) -> sigma(int_of_float (sub(n,e1)), int_of_float
    (sub(n, e2)), e3)
    | INTEGRAL(e1,e2,e3) -> integral(sub(n, e1), sub(n, e2), e3)
  and
  sigma(a,b,e) = 
    if a>b then 0.0
    else sub((float_of_int a),e) +. sigma(a + 1,b,e)
  and
  integral(a,b,e) = 
    let dx= 0.1 in
    let rec integral'(a,b,e) = 
    if a>=b then 0.0
    else
      sub(a,e)*.dx +. integral'(a +.dx, b, e)
    in
    if a > b then integral'(b,a,e) *. (-1.0)
    else integral'(a,b,e)
  in
  match e with
  | X -> raise FreeVariable
  | INT i -> float_of_int i
  | REAL f -> f
  | ADD(e1,e2) -> (mathemadiga e1) +. (mathemadiga e2)
  | SUB(e1,e2) -> (mathemadiga e1) -. (mathemadiga e2)
  | MUL(e1,e2) -> (mathemadiga e1) *. (mathemadiga e2)
  | DIV(e1,e2) -> (mathemadiga e1) /. (mathemadiga e2)
  | SIGMA(e1,e2,e3) -> sigma(int_of_float (mathemadiga e1), int_of_float
  (mathemadiga e2), e3)
  | INTEGRAL(e1,e2,e3) -> integral(mathemadiga e1, mathemadiga e2, e3)

