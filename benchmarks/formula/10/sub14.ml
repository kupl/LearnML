type expr = NUM of int
          | PLUS of expr * expr
          | MINUS of expr * expr
type formula = TRUE
             | FALSE
             | NOT of formula
             | ANDALSO of formula * formula
             | ORELSE of formula * formula
             | IMPLY of formula * formula
             | LESS of expr * expr

let rec eval p =
    let rec evalexpr e = match e with
		NUM n -> n
	  | PLUS (e1, e2) -> (evalexpr e1) + (evalexpr e2)
	  | MINUS (e1, e2) -> (evalexpr e1) - (evalexpr e2)
	in

    match p with
    TRUE -> true
  | FALSE -> false
  | NOT p1 -> if eval p1 then false else true
  | ANDALSO (p1, p2) -> if eval p1 then if eval p2 then true else false else false
  | ORELSE (p1, p2) -> if eval p1 then true else if eval p2 then true else false
  | IMPLY (p1, p2) -> if eval p1 then if eval p2 then true else false else true
  | LESS (e1, e2) -> (evalexpr e1) < (evalexpr e2)