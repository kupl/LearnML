exception FreeVariable
exception My_exception

type exp = X
| INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp

let galculator exp =

  let get_value_float exp =
   match exp with
    | INT i -> float_of_int i
    | REAL f -> f
    | _ -> raise My_exception
  in

  let get_value_int exp =
  match exp with
  | INT i -> i
  | REAL f -> int_of_float f
  | _ -> raise My_exception
  in

  let rec loop_integral s e exp r f =
    if (s > e) || (e -. s < 0.1) then (REAL (0.1 *. r))
    else
      loop_integral (s+.(0.1)) e exp (r +. (get_value_float (f s true exp))) f 
  in

  let rec loop_sigma s e exp r f =
    if (s > e) then (REAL r)
    else
      loop_sigma (s+.(1.0)) e exp (r +. (get_value_float (f s true exp))) f 
  in

  let rec cal value v_flag exp =
  match exp with
  | X -> if v_flag == false then raise FreeVariable else REAL value
  | INT i -> REAL (float_of_int i)
  | REAL f -> REAL f
  | ADD (e1, e2) ->
      let num1 = (cal value v_flag e1) in
      let num2 = (cal value v_flag e2) in
      REAL ((get_value_float num1) +. (get_value_float num2))
  | SUB (e1, e2) -> 
      let num1 = (cal value v_flag e1) in
      let num2 = (cal value v_flag e2) in
      REAL ((get_value_float num1) -. (get_value_float num2))
  | MUL (e1, e2) ->
      let num1 = (cal value v_flag e1) in
      let num2 = (cal value v_flag e2) in
      REAL ((get_value_float num1) *. (get_value_float num2))
  | DIV (e1, e2) ->
      let num1 = (cal value v_flag e1) in
      let num2 = (cal value v_flag e2) in
      REAL ((get_value_float num1) /. (get_value_float num2))
  | SIGMA (e1, e2, e3) ->
      let first = float_of_int(get_value_int (cal 0.0 false e1)) in
      let last = float_of_int(get_value_int (cal 0.0 false e2)) in
      
      loop_sigma first last e3 (0.0) cal

  | INTEGRAL (e1, e2, e3) -> 
      let first = (get_value_float (cal 0.0 false e1)) in
      let last = (get_value_float (cal 0.0 false e2)) in
      
      if first > last then REAL ((get_value_float (loop_integral last first e3 (0.0) cal))*.(-1.0))
      else loop_integral first last e3 (0.0) cal
  in

get_value_float (cal 0.0 false exp)

