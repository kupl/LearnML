(*
 * Programming Languages, 2013 Fall.
 * Solution for Homework 2 : Galculator.
 * Joonwon Choi (jwchoi@ropas.snu.ac.kr)
 * Misc changes by Jaeseung Choi (2015 TA)
 *)

exception FreeVariable

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

let rec galc_with_x : exp -> float option -> float = fun e x_opt ->
  match e with
    |X ->
      (match x_opt with
      | None -> raise FreeVariable
      | Some s -> s
      )
    |INT i -> float_of_int i
    |REAL f -> f
    |ADD (e1, e2) -> (galc_with_x e1 x_opt) +. (galc_with_x e2 x_opt)
    |SUB (e1, e2) -> (galc_with_x e1 x_opt) -. (galc_with_x e2 x_opt)
    |MUL (e1, e2) -> (galc_with_x e1 x_opt) *. (galc_with_x e2 x_opt)
    |DIV (e1, e2) -> (galc_with_x e1 x_opt) /. (galc_with_x e2 x_opt)
    |SIGMA (e1, e2, e3) -> 
      let v1 = galc_with_x e1 x_opt in
      let v2 = galc_with_x e2 x_opt in
      let n = int_of_float v2 in

      let rec sigma_helper i accum_res =
        if i > n then 
          accum_res
	    else
          let v = (galc_with_x e3 (Some (float_of_int i))) in
          sigma_helper (i + 1) (v +. accum_res)
	  in
      sigma_helper (int_of_float v1) 0.0
    |INTEGRAL (e1, e2, e3) ->
      let v1 = galc_with_x e1 x_opt in
      let v2 = galc_with_x e2 x_opt in
      let rec integral_helper low_bound up_bound accum_res = 
        if (low_bound +. 0.1) > up_bound then accum_res
	  else
        let v = galc_with_x e3 (Some low_bound) in
        integral_helper (low_bound +. 0.1) up_bound (accum_res +. v *. 0.1)
	  in
      if v1 < v2 then 
        integral_helper v1 v2 0.0
      else
        -. (integral_helper v2 v1 0.0)

let galculator e = galc_with_x e None

