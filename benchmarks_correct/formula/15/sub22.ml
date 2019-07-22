type formula = TRUE
        |FALSE
        |NOT of formula
        |ANDALSO of formula * formula
        |ORELSE of formula * formula
        |IMPLY of formula * formula
        |LESS of expr * expr
and expr = NUM of int
        |PLUS of expr * expr
        |MINUS of expr * expr

let rec eval f : bool =
	match f with
	TRUE	-> true
	|FALSE	-> false
	|NOT(form)		-> not (eval form)
	|ANDALSO(form, latt)	-> (eval form) && (eval latt)
	|ORELSE(form, latt)	-> (eval form) || (eval latt)
	|IMPLY(form, latt)	-> not (eval form) || (eval latt)
	|LESS(form, latt)	-> if getval form < getval latt then true else false
and getval s = 
	match s with
	NUM(n)	-> n
	|PLUS(n, m)	-> (getval n) + (getval m)
	|MINUS(n, m)	-> (getval n) - (getval m)
