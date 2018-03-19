type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec sigma_fun(a, b, c) = 
if a < b then (match c with
  | X -> a
  | INT q -> q
  | ADD (q, w) -> sigma_fun(a, a, q) + sigma_fun(a, a, w) + sigma_fun(a+1, b, c)
  | SUB (q, w) -> sigma_fun(a, a, q) - sigma_fun(a, a, w) + sigma_fun(a+1, b, c)
  | MUL (q, w) -> sigma_fun(a, a, q) * sigma_fun(a, a, w) + sigma_fun(a+1, b, c)
  | DIV (q, w) -> sigma_fun(a, a, q) / sigma_fun(a, a, w) + sigma_fun(a+1, b, c)
  | SIGMA(INT q, INT w, e) -> sigma_fun(q, w, e) + sigma_fun(a+1, b, c)
  | _ -> 0)
else if a = b then(match c with
  | X -> a 
  | INT q -> q
  | ADD (q, w) -> sigma_fun(a, a, q) + sigma_fun(a, a, w)
  | SUB (q, w) -> sigma_fun(a, a, q) - sigma_fun(a, a, w)
  | MUL (q, w) -> sigma_fun(a, a, q) * sigma_fun(a, a, w)
  | DIV (q, w) -> sigma_fun(a, a, q) / sigma_fun(a, a, w)
  | SIGMA(INT q, INT w, e) -> sigma_fun(q, w, e)
  | _ -> 0)
else 0

let calculator : exp -> int
=fun e -> match e with
  | INT a -> a
  | ADD (INT a, INT b) -> a + b
  | SUB (INT a, INT b) -> a - b
  | MUL (INT a, INT b) -> a * b
  | DIV (INT a, INT b) -> a / b
  | SIGMA (INT a, INT b, c) -> sigma_fun(a, b, c)
  | _ -> 0