type exp = X
         | INT of int
         | REAL of float
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp
         | INTEGRAL of exp * exp * exp
;;

type bound = UNBOUND | REAL of float | INT of int
;;

type bound_exp = ADD of bound * bound
               | SUB of bound * bound
               | MUL of bound * bound
               | DIV of bound * bound
;;
exception FreeVariable;;


let four_operation_bound bound_exp =
  match bound_exp with
  | ADD (INT int_a, INT int_b) ->
     INT (int_a + int_b)
  | ADD (INT int_a, REAL real_b) ->
     REAL (float_of_int int_a +. real_b)
  | ADD (REAL real_a, INT int_b) ->
     REAL (real_a +. float_of_int int_b)
  | ADD (REAL real_a, REAL real_b) ->
     REAL (real_a +. real_b)
  | ADD (_, _) ->
     raise FreeVariable

  | SUB (INT int_a, INT int_b) ->
     INT (int_a - int_b)
  | SUB (INT int_a, REAL real_b) ->
     REAL (float_of_int int_a -. real_b)
  | SUB (REAL real_a, INT int_b) ->
     REAL (real_a -. float_of_int int_b)
  | SUB (REAL real_a, REAL real_b) ->
     REAL (real_a -. real_b)
  | SUB (_, _) ->
     raise FreeVariable

  | MUL (INT int_a, INT int_b) ->
     INT (int_a * int_b)
  | MUL (INT int_a, REAL real_b) ->
     REAL (float_of_int int_a *. real_b)
  | MUL (REAL real_a, INT int_b) ->
     REAL (real_a *. float_of_int int_b)
  | MUL (REAL real_a, REAL real_b) ->
     REAL (real_a *. real_b)
  | MUL (_, _) ->
     raise FreeVariable

  | DIV (INT int_a, INT int_b) ->
     INT (int_a / int_b)
  | DIV (INT int_a, REAL real_b) ->
     REAL (float_of_int int_a /. real_b)
  | DIV (REAL real_a, INT int_b) ->
     REAL (real_a /. float_of_int int_b)
  | DIV (REAL real_a, REAL real_b) ->
     REAL (real_a /. real_b)
  | DIV (_, _) ->
     raise FreeVariable
;;

let rec sigma_bound bound_a bound_b exp =
  match (bound_a, bound_b) with
  | (REAL real_a, _) ->
     let int_a = int_of_float real_a in
     let re_real_a = float_of_int int_a in

     if re_real_a < real_a then
       sigma_bound (INT (int_a + 1)) bound_b exp
     else
       sigma_bound (INT int_a) bound_b exp

  | (_, REAL real_b) ->
     let int_b = int_of_float real_b in
     let re_real_b = float_of_int int_b in

     if re_real_b > real_b then
       sigma_bound bound_a (INT (int_b - 1)) exp
     else
       sigma_bound bound_a (INT int_b) exp

  | (INT int_a, INT int_b) when int_a <= int_b ->
     let calc_exp = _galculator bound_a exp in
     let else_sigma = sigma_bound (INT (int_a + 1)) bound_b exp in
     four_operation_bound (ADD (calc_exp, else_sigma))
  | (_, _) ->
     (REAL 0.0)

and integral_bound bound_a bound_b exp =
  let width = 0.1 in
  match (bound_a, bound_b) with
  | (INT int_a, _) ->
     integral_bound (REAL (float_of_int int_a)) bound_b exp
  | (_, INT int_b) ->
     integral_bound bound_a (REAL (float_of_int int_b)) exp

  | (REAL real_a, REAL real_b) when (real_a > real_b) ->
     let reverse_integral = integral_bound bound_b bound_a exp in
     four_operation_bound (SUB (INT 0, reverse_integral))

  | (REAL real_a, REAL real_b) when (real_a < real_b && (real_a +. width) <= real_b) ->
     let calc_exp = _galculator bound_a exp in
     let calc_area = four_operation_bound (MUL (calc_exp, (REAL width))) in
     let else_integral = integral_bound (REAL (real_a +. width)) bound_b exp in
     four_operation_bound (ADD (calc_area, else_integral))
  | (_, _) ->
     (REAL 0.0)

and _galculator bound exp =
  let bound_galculator = _galculator bound in
  match exp with
  | X ->
     (match bound with
      | UNBOUND -> raise FreeVariable
      | _ -> bound)
  | ADD (a, b) ->
     let calc_a = bound_galculator a in
     let calc_b = bound_galculator b in
     four_operation_bound (ADD (calc_a, calc_b))
  | SUB (a, b) ->
     let calc_a = bound_galculator a in
     let calc_b = bound_galculator b in
     four_operation_bound (SUB (calc_a, calc_b))
  | MUL (a, b) ->
     let calc_a = bound_galculator a in
     let calc_b = bound_galculator b in
     four_operation_bound (MUL (calc_a, calc_b))
  | DIV (a, b) ->
     let calc_a = bound_galculator a in
     let calc_b = bound_galculator b in
     four_operation_bound (DIV (calc_a, calc_b))
  | SIGMA (a, b, exp) ->
     let calc_a = bound_galculator a in
     let calc_b = bound_galculator b in
     sigma_bound calc_a calc_b exp
  | INTEGRAL (a, b, exp) ->
     let calc_a = bound_galculator a in
     let calc_b = bound_galculator b in
     integral_bound calc_a calc_b exp
  | INT int ->
     INT int
  | REAL float ->
     REAL float
;;


let galculator exp =

  let result = _galculator UNBOUND exp in

  match result with
  | INT int ->
     float_of_int int
  | REAL float ->
     float
  | UNBOUND ->
     raise FreeVariable
;;
