type formula =
    True
  | False
  | Neg of formula
  | Or of formula * formula
  | And of formula * formula
  | Imply of formula * formula
  | Equiv of formula * formula

let rec eval : formula -> bool
=fun f -> 
match f with
|True -> true
|False-> false
|Neg f1 -> (match eval f1 with
		|true -> false
		|false -> true)
|Or (form1,form2) -> (eval form1) || (eval form2)
|And (form1,form2) -> (eval form1) && (eval form2)
|Imply (form1,form2) ->
	 (match eval form1 with
		|true -> eval form2
		|false -> true)
|Equiv (form1,form2) ->
	if eval form1=eval form2 then true else false
