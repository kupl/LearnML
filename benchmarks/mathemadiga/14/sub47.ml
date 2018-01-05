

(* Author: Arif Jafer, 2012-11255 *)
(* PL, Spring 2014 *)

(* HW2-Q2: Galculator *)

type exp =
  | X
  | INT of int
  | REAL of float
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
  | INTEGRAL of exp * exp * exp ;;


let dx = 0.1 ;;
exception X_NOT_SPECIFIED ;;

let rec sigma a b f accm =
  if (a > b) then
    (accm +. 0.0)
  else
    (sigma (a +. 1.) b f (accm +. (f a))) ;;

let rec integrate a b f accm is_first_time =
  if (is_first_time && (a > b))
  then (integrate b a f accm false)
  else if (a > b)
  then (accm +. 0.0)
  else (integrate (a +. dx) b f (accm +. ((f a) *. dx)) false) ;;

let initial_x =
  fun () -> raise X_NOT_SPECIFIED


let galculator exp =
  let rec iter exp x =
    match exp with
    | X                          -> x()
    | INT i                      -> float_of_int i
    | REAL i                     -> i
    | ADD (e1, e2)               -> (iter e1 x) +. (iter e2 x)
    | SUB (e1, e2)               -> (iter e1 x) -. (iter e2 x)
    | MUL (e1, e2)               -> (iter e1 x) *. (iter e2 x)
    | DIV (e1, e2)               -> (iter e1 x) /. (iter e2 x)
    | SIGMA (from, till, e)      -> sigma (iter from x)
                                          (iter till x)
                                          (fun v -> (iter e (fun () -> v)))
                                          0.0
    | INTEGRAL (from, till, e)   -> integrate (iter from x)
                                              (iter till x)
                                              (fun v -> (iter e (fun () -> v)))
                                              0.0
                                              true
  in
  iter exp initial_x ;;


