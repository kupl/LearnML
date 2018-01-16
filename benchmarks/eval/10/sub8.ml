exception DividedByZero

type expr = NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr
  | MULT of expr * expr
  | DIVIDE of expr * expr
  | MAX of expr list

let rec eval e =
  match e with NUM i -> i
  | PLUS (a, b) -> (eval a) + (eval b)
  | MINUS (a, b) -> (eval a) - (eval b)
  | MULT (a, b) -> (eval a) * (eval b)
  | DIVIDE (a, b) ->
  let c = (eval b) in
  if c = 0 then raise DividedByZero
  else (eval a) / c
  | MAX lst -> 
  let rec max lst =
  match lst with [] -> 0
  | [a] -> (eval a)
  | h::t ->
  let hh = (eval h) in
  let tt = if t = [] then min_int else max t in
  if hh > tt then hh else tt
  in
  max lst
