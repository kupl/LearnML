(*
	department : computer science & engineering
	student ID : 2012-11242 / name : Seon-bi, Park
*)

type formula = TRUE
			| FALSE
			| NOT of formula
			| ANDALSO of formula * formula
			| ORELSE of formula * formula
			| IMPLY of formula * formula
			| LESS of expr * expr
and expr = NUM of int | PLUS of expr * expr | MINUS of expr * expr


let rec cal num =		(* expr -> int *)
	match num with
		| NUM (lnum) -> lnum
		| PLUS (lnum, rnum) -> (cal (lnum) + cal (rnum))
		| MINUS (lnum, rnum) -> (cal (lnum) - cal (rnum))


let rec eval logic =		(* formula -> bool *)
	match logic with
		| TRUE -> true
		| FALSE -> false
		| NOT (fst) -> (not (eval (fst)))
		| ANDALSO (fst, snd) -> ((eval (fst)) && (eval (snd)))
		| ORELSE (fst, snd) -> ((eval (fst)) || (eval (snd)))
		| IMPLY (fst, snd) -> 
			( if (eval (fst) = false) then true
			  else if (eval (snd) = true) then true
			  else false
			  )
		| LESS (fst, snd) -> 
			( if cal (fst) < cal (snd) then true
			  else false
			  )
