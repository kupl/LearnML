(* Department: EE
 * Student No.: 2009-20769
 * Name: Kim, Seongjun
 * Exercise 5
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

type var = UNBINDED
	   | VAR of float

exception FreeVariable
exception InvalidSigma
exception DivideByZero


let mathemadiga (exp:exp) =
  let rec var_series (first:float) (last:float) (interval:float) =
    if first > last then
      []
    else 
      (VAR first) :: (var_series (first +. interval) last interval)
  in
  let get_last (xs:var list) =
    let rev = List.rev xs in
      (List.hd rev, List.rev (List.tl rev))
  in
  let float_of_var (x:var) =
    match x with
	UNBINDED -> raise FreeVariable
      | VAR f -> f
  in
  let rec eval (exp:exp) (x:var) =
    match exp with
	X -> (
	  match x with
	      UNBINDED -> raise FreeVariable
	    | VAR v -> v
	)
      | INT i -> float_of_int i
      | REAL r -> r
      | ADD (op1, op2) -> (eval op1 x) +. (eval op2 x)
      | SUB (op1, op2) -> (eval op1 x) -. (eval op2 x)
      | MUL (op1, op2) -> (eval op1 x) *. (eval op2 x)
      | DIV (op1, op2) ->
	  let float_of_op2 = eval op2 x in
	    if float_of_op2 = 0.0 then
	      raise DivideByZero
	    else
	      (eval op1 x) /. float_of_op2

      | SIGMA (f, l, op) -> 
          let first = eval f x in
	  let last = eval l x in
	    if first > last then
	      raise InvalidSigma
	    else
	      List.fold_left (+.) 0.0 (List.map (eval op) (var_series first last 1.0))

      | INTEGRAL (f, l, op) ->
	  let first = eval f x in
	  let last = eval l x in
	    if first = last then
	      0.0
	    else if first < last then
	      let vs = var_series first last 0.1 in
	      let (vn, vs) = get_last vs in
		(List.fold_left (+.) 0.0 (List.map (( *. ) 0.1) (List.map (eval op) vs)))
		+.
		  ((last -. (float_of_var vn)) *. (eval op vn))
	    else
	      (-1.0) *. (eval (INTEGRAL (l, f, op)) x)
  in
    eval exp UNBINDED



(*
let test_fv f arg1 =
  try
    f arg1;
    assert(false)
  with FreeVariable ->
    assert(true)
;;

let test_is f arg1 =
  try
    f arg1;
    assert(false)
  with InvalidSigma ->
    assert(true)
;;

let test_dz f arg1 =
  try
    f arg1;
    assert(false)
  with DivideByZero ->
    assert(true)
;;

let error_less (result:float) (expect:float) (limit:float) =
  assert (abs_float (result -. expect) < limit)
;;

(* test arithmatic operator *)
assert (3.0 = mathemadiga (ADD (REAL 1.0, REAL 2.0)));;
assert (4.1 = mathemadiga (SUB (REAL 5.1, REAL 1.0)));;
assert (1.44 = mathemadiga (MUL (REAL 1.2, REAL 1.2)));;
assert (1.2 = mathemadiga (DIV (REAL 1.44, REAL 1.2)));;

(* test SIGMA *)
assert (10.0 = mathemadiga (SIGMA (REAL 1.0, REAL 10.0, INT 1)));;
assert (55.0 = mathemadiga (SIGMA (REAL 1.0, REAL 10.0, X)));;
assert (55.0 = mathemadiga (SIGMA (REAL 1.0, REAL 10.9, X)));;

(* test INTEGRAL *)
error_less (mathemadiga (INTEGRAL (REAL 1.0, REAL 2.0, INT 1))) 1.0 0.1;;
error_less (mathemadiga (INTEGRAL (REAL 1.0, REAL 2.0, X))) 1.5 0.1;;
error_less (mathemadiga (INTEGRAL (REAL 2.0, REAL 1.0, X))) (-1.5) 0.1;;
error_less (mathemadiga (INTEGRAL (REAL 2.0, REAL 2.0, X))) 0.0 0.1;;

(* test SIGMA - INTEGRAL composition *)
assert (18.0 = mathemadiga (SIGMA (INT 1, INT 3, SIGMA (INT 1, INT 3, X))));;
mathemadiga (SIGMA (INT 1, INT 3, SIGMA(INT 1, INT 3, X)));;

(* test exception Invalidsigma *)
test_is mathemadiga (SIGMA (REAL 5.0, REAL 1.0, X));;
assert (1.0 = mathemadiga (SIGMA (REAL 0.0, REAL 1.0, X)));;

(* test exception DivideByZero *)
test_dz mathemadiga (DIV (REAL 4.0, REAL 0.0));;
assert (2.0 = mathemadiga (DIV (REAL 4.0, REAL 2.0)));;

(* test exception FreeVariable *)
test_fv mathemadiga (ADD (REAL 4.0, X));;
*)
