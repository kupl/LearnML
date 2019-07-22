
type formula = TRUE
| FALSE
| NOT of formula
| ANDALSO of formula * formula
| ORELSE of formula * formula
| IMPLY of formula * formula
| LESS of expr * expr
and expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr

let rec calc (e : expr) : int =
	match e with
	| NUM i -> i
	| PLUS (ea, eb) -> (calc ea) + (calc eb)
	| MINUS (ea, eb) -> (calc ea) - (calc eb)

let rec eval (f : formula) : bool =
	match f with
	| TRUE -> true
	| FALSE -> false
	| NOT fa -> not (eval fa)
	| ANDALSO (fa, fb) -> (match (eval fa) with
												| true -> eval fb
												| _ -> false)
	| ORELSE (fa, fb) -> (match (eval fa) with
												| true -> true
												| _ -> eval fb)
	| IMPLY (fa, fb) -> (match (eval fa) with
												| true -> eval fb
												| _ -> true)
	| LESS (ea, eb) -> (match compare (calc ea) (calc eb) with
											| -1 -> true
											| _ -> false)
