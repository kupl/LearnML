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

let rec galculatorvariable (ex, v_list) = 
  match (ex, v_list) with
  | (X,[]) -> raise FreeVariable
  | (X, head::tail) -> head
  | (INT a, _) -> float_of_int a
  | (REAL a, _) -> a
  | (ADD (a,b), _) -> 
      galculatorvariable(a, v_list) +. galculatorvariable(b, v_list)
  | (SUB (a,b), _) ->
      galculatorvariable(a, v_list) -. galculatorvariable(b, v_list)
  | (MUL (a,b), _) ->
      galculatorvariable(a, v_list) *. galculatorvariable(b, v_list)
  | (DIV (a,b), _) ->
      galculatorvariable(a, v_list) /. galculatorvariable(b, v_list)
  | (SIGMA (a,b,c), v_list) -> (galsigma (galculatorvariable (a, v_list),
  galculatorvariable (b, v_list),c,v_list))
  | (INTEGRAL (a,b,c), v_list) -> (galintegral
  (galculatorvariable(a,v_list),galculatorvariable(b,v_list),c,v_list))

and  galsigma (a,b,c,v_list) = 
  if (a>b) then
    0.
  else
    galsigma (a+.1.,b,c,v_list) +. galculatorvariable (c, a::v_list)
    
and galintegral (a,b,c,v_list) = 
  let rec galintegral_cal (a,b,c,v_list) =
    if (a +. 0.1>b) then
      0.
    else
      galintegral_cal(a+.0.1,b,c,v_list) +. galculatorvariable (c, a::v_list)
      *. 0.1
  in
  if(a>b) then
    galintegral_cal(b,a,c,v_list) *. -1.
  else
    galintegral_cal(a,b,c,v_list)


let rec galculator ex = 
  galculatorvariable(ex,[]);; 
