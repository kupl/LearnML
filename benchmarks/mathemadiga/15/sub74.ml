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

let rec galculatorX e x  = match e with
  | X -> x
  | INT k -> float_of_int k
  | REAL k -> k
  | ADD (k, q) -> galculatorX k x +. galculatorX q x
  | SUB (k, q) -> galculatorX k x -. galculatorX q x
  | MUL (k, q) -> galculatorX k x *. galculatorX q x 
  | DIV (k, q) -> galculatorX k x /. galculatorX q x
  | SIGMA (a, b, f) -> let ret = ref 0.0 in
    let ax = int_of_float(galculatorX a x) in
    let bx = int_of_float(galculatorX b x) in

    for i = min ax bx to max ax bx do
      ret := !ret +. (galculatorX f (float_of_int i));
    done;
    if ax<bx then !ret else 0.0
  | INTEGRAL (a, b, f) -> let ret = ref 0.0 in
      let ax = galculatorX a x in
      let bx = galculatorX b x in
      let i = ref (min ax bx) in
      let bk = max ax bx in
      while (!i+.0.1) <= bk do
        ret := !ret +. (galculatorX f !i) *. 0.1;
        i := !i +. 0.1;
      done;
      if ax < bx then !ret else -. !ret

let rec galculator e = match e with
  | X -> raise FreeVariable
  | INT k -> float_of_int k
  | REAL k -> k
  | ADD (k, q) -> galculator k +. galculator q
  | SUB (k, q) -> galculator k -. galculator q
  | MUL (k, q) -> galculator k *. galculator q
  | DIV (k, q) -> galculator k /. galculator q
  | SIGMA (a, b, f) -> let ret = ref 0.0 in
    let ax = int_of_float(galculator a) in
    let bx = int_of_float(galculator b) in
    for i = min ax bx to max ax bx do
      ret := !ret +. (galculatorX f (float_of_int i));
    done;
    if ax<bx then !ret else 0.0
  | INTEGRAL (a, b, f) -> let ret = ref 0.0 in
      let ax = galculator a in
      let bx = galculator b in
      let i = ref (min ax bx) in
      let bk = max ax bx in
      while (!i+.0.1) <= bk do
        ret := !ret +. (galculatorX f !i)*. 0.1;
        i := !i +. 0.1;
      done;
      if ax < bx then !ret else -. !ret

