(*
	CSE / 2013-11426 / Im DongYeop
	Homework 2: Exercise 1
*)

type formula = True
						 | False
						 | Not of formula
						 | AndAlso of formula * formula
						 | OrElse of formula * formula
						 | Imply of formula * formula
						 | Equal of exp * exp
and exp = Num of int
				 | Plus of exp * exp
				 | Minus of exp * exp

let rec gonum(e: exp): exp = 
	match e with
	| Num ein -> Num ein
	| Plus (ein1, ein2) ->
		(match (ein1, ein2) with
		| (Num eein1, Num eein2) -> Num(eein1 + eein2)
		| _ -> gonum(Plus(gonum(ein1), gonum(ein2))))
	| Minus (ein1, ein2) ->
		(match (ein1, ein2) with
		| (Num eein1, Num eein2) -> Num(eein1 - eein2)
		| _ -> gonum(Minus(gonum(ein1), gonum(ein2))))

let rec goform(f: formula): formula =
	match f with
	| True -> True
	| False -> False
	| Not ff ->
		(match ff with
		| True -> False
		| False -> True
		| _ -> Not(goform(ff)))
	| AndAlso (f1, f2) ->
		(match (f1, f2) with
		| (True, True) -> True
		| (False, _) -> False
		| (_, False) -> False
		| _ -> AndAlso(goform(f1), goform(f2)))
	| OrElse (f1, f2) ->
		(match (f1, f2) with
		| (False, False) -> False
		| (True, _) -> True
		| (_, True) -> True
		| _ -> OrElse(goform(f1), goform(f2)))
	| Imply (f1, f2) ->
		(match (f1, f2) with
		| (True, False) -> False
		| (_, True) -> True
		| (False, False) -> True
		| _ -> Imply(goform(f1), goform(f2)))
	| Equal (e1, e2) ->
		(match (e1, e2) with
		| (Num ein1, Num ein2) -> (
			if ein1 = ein2	then	True
			else	False)
		| _ -> Equal(gonum(e1), gonum(e2)))

let rec eval(f: formula): bool =
	match f with
	| True -> true
	| False -> false
	| Not ff-> eval(goform(Not ff))
	| AndAlso (f1, f2) -> eval(goform(AndAlso (f1, f2)))
	| OrElse (f1, f2) -> eval(goform(OrElse (f1, f2)))
	| Imply (f1, f2) -> eval(goform(Imply (f1, f2)))
	| Equal (e1, e2) ->
		(match (e1, e2) with
		| (Num ein1, Num ein2) -> ein1 = ein2
		| _ -> eval(Equal(gonum(e1), gonum(e2))))

