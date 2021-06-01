(*
	department : computer science & engineering
	student ID : 2012-11242 / name : Seon-bi, Park
*)

type formula = True
			| False
			| Not of formula
			| AndAlso of formula * formula
			| OrElse of formula * formula
			| Imply of formula * formula
			| Equal of exp * exp
and exp = Num of int | Plus of exp * exp | Minus of exp * exp


let rec cal num =		(* exp -> int *)
	match num with
		| Num (lnum) -> lnum
		| Plus (lnum, rnum) -> (cal (lnum) + cal (rnum))
		| Minus (lnum, rnum) -> (cal (lnum) - cal (rnum))


let rec eval logic =		(* formula -> bool *)
	match logic with
		| True -> true
		| False -> false
		| Not (fst) -> (not (eval (fst)))
		| AndAlso (fst, snd) -> ((eval (fst)) && (eval (snd)))
		| OrElse (fst, snd) -> ((eval (fst)) || (eval (snd)))
		| Imply (fst, snd) -> 
			( if (eval (fst) = false) then true
			  else if (eval (snd) = true) then true
			  else false
			  )
		| Equal (fst, snd) -> 
			( if cal (fst) = cal (snd) then true
			  else false
			  )
