type formula =
  TRUE
| FALSE
| NOT of formula
| ANDALSO of formula * formula
| ORELSE of formula * formula
| IMPLY of formula * formula
| LESS of expr * expr
and expr = 
  NUM of int
| PLUS of expr * expr
| MINUS of expr * expr

let rec ecal e =
  match e with
    NUM i -> i
  | PLUS (i, j) -> (ecal i) + (ecal j)
  | MINUS (i, j) -> (ecal i) - (ecal j)  

let rec eval p =
  match p with
    TRUE -> true
  | FALSE -> false
  | NOT p -> 
	if (eval p) == true then false else true
  | ANDALSO (p, q) -> 
	if (((eval p) == true) && ((eval q) == true)) then true else false
  | ORELSE (p, q) ->
	if (((eval p) == false) && ((eval q) == false)) then false else true
  | IMPLY (p, q) ->
	if (((eval p) == true) && ((eval q) == false)) then false else true
  | LESS (e1, e2) ->
	if ((ecal e1) < (ecal e2)) then true else false


