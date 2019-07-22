(* C:\Users\saigoy\Desktop\eval.ml *)

type formula = TRUE
  | FALSE
  | NOT of formula
  | ANDALSO of formula * formula
  | ORELSE of formula * formula
  | IMPLY of formula * formula
  | LESS of expr * expr
  and expr = NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr;;

let rec eval : formula -> bool = fun fml ->
  	let rec calc exp = 
  	match exp with
  	| NUM n -> n
  	| PLUS (le, re) -> ((calc le) + (calc re))
  	| MINUS (le, re) -> ((calc le) - (calc re)) in
  match fml with
  | TRUE -> true 
  | FALSE -> false
  | NOT f -> (eval f)
  | ANDALSO (lf, rf) -> ((eval lf)&&(eval rf))
  | ORELSE (lf, rf) -> ((eval lf)||(eval rf))
  | IMPLY (lf, rf) -> 
  (
  	if ( (eval lf) ) then (eval rf)
  	else true
  )
  | LESS (le, re) -> ((calc le) < (calc re));;

