(*
	CSE / 2013-11426 / Im DongYeop
	Homework 2: Exercise 1
*)

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

let rec gonum(e: expr): expr = 
	match e with
	| NUM ein -> NUM ein
	| PLUS (ein1, ein2) ->
		(match (ein1, ein2) with
		| (NUM eein1, NUM eein2) -> NUM(eein1 + eein2)
		| _ -> gonum(PLUS(gonum(ein1), gonum(ein2))))
	| MINUS (ein1, ein2) ->
		(match (ein1, ein2) with
		| (NUM eein1, NUM eein2) -> NUM(eein1 - eein2)
		| _ -> gonum(MINUS(gonum(ein1), gonum(ein2))))

let rec goform(f: formula): formula =
	match f with
	| TRUE -> TRUE
	| FALSE -> FALSE
	| NOT ff ->
		(match ff with
		| TRUE -> FALSE
		| FALSE -> TRUE
		| _ -> NOT(goform(ff)))
	| ANDALSO (f1, f2) ->
		(match (f1, f2) with
		| (TRUE, TRUE) -> TRUE
		| (FALSE, _) -> FALSE
		| (_, FALSE) -> FALSE
		| _ -> ANDALSO(goform(f1), goform(f2)))
	| ORELSE (f1, f2) ->
		(match (f1, f2) with
		| (FALSE, FALSE) -> FALSE
		| (TRUE, _) -> TRUE
		| (_, TRUE) -> TRUE
		| _ -> ORELSE(goform(f1), goform(f2)))
	| IMPLY (f1, f2) ->
		(match (f1, f2) with
		| (TRUE, FALSE) -> FALSE
		| (_, TRUE) -> TRUE
		| (FALSE, FALSE) -> TRUE
		| _ -> IMPLY(goform(f1), goform(f2)))
	| LESS (e1, e2) ->
		(match (e1, e2) with
		| (NUM ein1, NUM ein2) -> (
			if ein1 < ein2	then	TRUE
			else	FALSE)
		| _ -> LESS(gonum(e1), gonum(e2)))

let rec eval(f: formula): bool =
	match f with
	| TRUE -> true
	| FALSE -> false
	| NOT ff-> eval(goform(NOT ff))
	| ANDALSO (f1, f2) -> eval(goform(ANDALSO (f1, f2)))
	| ORELSE (f1, f2) -> eval(goform(ORELSE (f1, f2)))
	| IMPLY (f1, f2) -> eval(goform(IMPLY (f1, f2)))
	| LESS (e1, e2) ->
		(match (e1, e2) with
		| (NUM ein1, NUM ein2) -> ein1 < ein2
		| _ -> eval(LESS(gonum(e1), gonum(e2))))

