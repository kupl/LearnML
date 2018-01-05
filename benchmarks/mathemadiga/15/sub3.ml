(*
    PL 2-5
    2008-11609 박성원
*)

type exp = X
         | INT of int
         | REAL of float
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp
         | INTEGRAL of exp * exp * exp
exception FreeVariable

let galculator exp =
  let rec calc exp x =
    match exp with
    | X -> (
      match x with
      | None -> raise FreeVariable
      | Some r -> r )
    | INT n -> float_of_int n
    | REAL r -> r
    | ADD (e1, e2) -> (calc e1 x) +. (calc e2 x)
    | SUB (e1, e2) -> (calc e1 x) -. (calc e2 x)
    | MUL (e1, e2) -> (calc e1 x) *. (calc e2 x)
    | DIV (e1, e2) -> (calc e1 x) /. (calc e2 x)
    | SIGMA (a, b, exp) -> sigmaLoop (int_of_float (calc a x)) (int_of_float (calc b x)) exp 0.
    | INTEGRAL (a, b, exp) -> integralLoop (calc a x) (calc b x) exp 0.
  and sigmaLoop cur ed func result =
    if cur > ed then
      result
    else
      sigmaLoop (cur + 1) ed func (result +. (calc func (Some (float_of_int cur))))
  and integralLoop cur ed func result =
    if cur > ed then
      ~-.(integralLoop ed cur func result)
    else if ed -. cur < 0.1 then
      result
    else
      integralLoop (cur +. 0.1) ed func (result +. (calc func (Some cur)) *. 0.1)
  in
  calc exp None
