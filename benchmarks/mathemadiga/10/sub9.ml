exception FreevarError
exception DividedByZero
exception NotAValue

type exp = X
  | INT of int
  | REAL of float
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
  | INTEGRAL of exp * exp * exp

let value v =
  match v with INT n -> float_of_int n
  | REAL r -> r
  | _ -> raise NotAValue

let rec calc (e, v) =
  match (e, v) with (X, k) -> value k
  | (INT n, k) -> float_of_int n
  | (REAL r, k) -> r
  | (ADD(a, b), k) -> calc(a, k) +. calc(b, k)
  | (SUB(a, b), k) -> calc(a, k) -. calc(b, k)
  | (MUL(a, b), k) -> calc(a, k) *. calc(b, k)
  | (DIV(a, b), k) ->
  let c = calc(b, k) in
  if c = 0.0 then raise DividedByZero else calc(a, k) /. c
  | (SIGMA (a, b, e), k) ->
  let aa = calc (a, k) in
  let bb = calc (b, k) in
  if aa -. (float_of_int (int_of_float aa)) = 0.0 && bb -. (float_of_int (int_of_float bb)) = 0.0
  then if aa = bb then calc(e, INT (int_of_float aa))
  else calc(e, INT (int_of_float aa)) +. calc (SIGMA (INT ((int_of_float aa)+1), b, e), k)
 (*    (mathemadiga (SIGMA (INT (int_of_float aa), INT (int_of_float bb), e)))    *)
  else raise NotAValue
  | (INTEGRAL (a, b, e), k) ->
  let aa = calc (a, k) in
  let bb = calc (b, k) in
  if aa > bb then 0.0 -. calc (INTEGRAL (b, a, e), k)
  else if aa = bb then 0.0
  else
  if bb -. aa >= 0.1 then (0.1 *. calc (e, REAL aa)) +. calc (INTEGRAL (REAL (aa +. 0.1), b, e), k)
  else (bb -. aa) *. calc (e, REAL aa)
  (* mathemadiga (INTEGRAL (REAL (calc (a, k)), REAL (calc (b, k)), e)) *)

let rec mathemadiga e =
  match e with X -> raise FreevarError
  | INT n -> float_of_int n
  | REAL r -> r
  | ADD (a, b) -> (mathemadiga a) +. (mathemadiga b)
  | SUB (a, b) -> (mathemadiga a) -. (mathemadiga b)
  | MUL (a, b) -> (mathemadiga a) *. (mathemadiga b)
  | DIV (a, b) ->
  let c = (mathemadiga b) in
  if c = 0.0 then raise DividedByZero else (mathemadiga a) /. c
  | SIGMA (INT rl, INT ru, exp) ->
  if rl = ru then calc (exp, INT ru)
  else calc(exp, INT rl) +. mathemadiga (SIGMA (INT (rl + 1), INT ru, exp))
  | SIGMA (_, _, exp) -> raise FreevarError
  | INTEGRAL (rl, ru, exp) ->
  match (rl, ru, exp) with (X, _, _) | (_, X, _) -> raise FreevarError
  | (a, b, e) ->
  let aa = value a in
  let bb = value b in
  if aa > bb then 0.0 -. mathemadiga (INTEGRAL (b, a, e))
  else if aa = bb then 0.0
  else
  if bb -. aa >= 0.1 then (0.1 *. calc (e, a)) +. mathemadiga (INTEGRAL (REAL (aa +. 0.1), b, e))
  else (bb -. aa) *. calc (e, a)