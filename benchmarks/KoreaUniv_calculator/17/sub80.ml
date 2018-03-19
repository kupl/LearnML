(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let calculator : exp -> int
= fun e -> (* TODO *)
let rec replace _e n =
  match _e with
  | X -> INT (n)
  | ADD (x, y) -> ADD ((replace x n), (replace y n))
  | SUB (x, y) -> SUB ((replace x n), (replace y n))
  | MUL (x, y) -> MUL ((replace x n), (replace y n))
  | DIV (x, y) -> DIV ((replace x n), (replace y n))
  | _ -> _e in
let rec impl _e =
  match _e with
  | INT (x) -> x
  | ADD (x, y) -> (impl x) + (impl y)
  | SUB (x, y) -> (impl x) - (impl y)
  | MUL (x, y) -> (impl x) * (impl y)
  | DIV (x, y) -> (impl x) / (impl y)
  | SIGMA (a, b, x) ->
  let _a = impl a in
  let _b = impl b in
  if _a == _b then impl (replace x _a)
  else
    if _a < _b then (impl (replace x _a)) + (impl (SIGMA ((INT (_a + 1)), INT (_b), x)))
    else (impl (replace x _b)) + (impl (SIGMA ((INT (_b + 1)), INT (_a), x)))
  | _ -> 0 in
impl e;;

