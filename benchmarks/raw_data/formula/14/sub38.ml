exception TODO

type formula = 
  | True
  | False
  | Not of formula
  | AndAlso of formula * formula
  | OrElse of formula * formula
  | Imply of formula * formula
  | Equal of exp * exp
and exp =
  | Num of int
  | Plus of exp * exp
  | Minus of exp * exp


let rec expeval (e: exp): int = 
	match e with
	| Num i -> i
	| Plus (e1, e2) -> (expeval e1) + (expeval e2)
	| Minus (e1, e2) -> (expeval e1) - (expeval e2)


let rec eval (f: formula): bool =
	match f with
	| True -> true
	| False -> false
	| Not g -> not (eval g)
	| AndAlso (f1, f2) -> eval f1 && eval f2
  | OrElse (f1, f2) -> eval f1 || eval f2
  | Imply (f1, f2) -> not (eval f1) || eval f2
  | Equal (f1, f2) -> 
		if expeval f1 = expeval f2 then true else false
		
		
		
	